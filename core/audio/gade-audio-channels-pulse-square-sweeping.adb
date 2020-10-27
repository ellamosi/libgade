package body Gade.Audio.Channels.Pulse.Square.Sweeping is

   overriding
   procedure Disable
     (Channel : in out Sweeping_Square_Channel;
      Mode    : Disable_Mode)
   is
   begin
      Parent (Channel).Disable (Mode);
      Channel.Sweep_Timer.Setup;
      Channel.Sweep_Enabled := False;
      if Mode = APU_Power_Off then
         Channel.NRx0 := NRx0_Sweep_Mask;
         Channel.Sweep_Negate := False;
         Channel.Sweep_Negated := False;
         Channel.Sweep_Period := 0;
         Channel.Sweep_Shift := 0;
      end if;
   end Disable;

   procedure Calculate_New_Frequency
     (Channel       : in out Sweeping_Square_Channel;
      New_Frequency : out Integer)
   is
      Shifted          : Integer;
      Shadow_Frequency : constant Natural := Natural (Channel.Shadow_Frequency);
   begin
      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Frequency_Sweep
      --
      --  Frequency calculation consists of taking the value in the frequency
      --  shadow register, shifting it right by sweep shift, optionally
      --  negating the value, and summing this with the frequency shadow
      --  register to produce a new frequency.
      Shifted := Shadow_Frequency / 2 ** Natural (Channel.Sweep_Shift);
      Shifted := (if Channel.Sweep_Negate then -Shifted else Shifted);

      New_Frequency := Shadow_Frequency + Shifted;
      Channel.Sweep_Negated := Channel.Sweep_Negated or Channel.Sweep_Negate;

      if New_Frequency > Max_Frequency then
         Channel.Disable (Self_Disable);
         --  Channel.Sweep_Enabled := False;
      end if;
   end Calculate_New_Frequency;

   procedure Step_Frequency_Sweep (Channel : in out Sweeping_Square_Channel) is
      New_Frequency : Integer;
   begin
      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Frequency_Sweep
      --
      --  The sweep timer is clocked at 128 Hz by the frame sequencer. When it
      --  generates a clock and the sweep's internal enabled flag is set and the
      --  sweep period is not zero, a new frequency is calculated and the
      --  overflow check is performed.
      if Channel.Sweep_Enabled and Channel.Sweep_Period /= 0 then
         Calculate_New_Frequency (Channel, New_Frequency);

         --  If the new frequency is 2047 or less and the sweep shift is not
         --  zero, this new frequency is written back to the shadow frequency
         --  and square 1's frequency in NR13 and NR14
         if New_Frequency <= Max_Frequency and Channel.Sweep_Shift /= 0 then
            Channel.Shadow_Frequency := Frequency_Type (New_Frequency);
            Channel.Set_Frequency (Channel.Shadow_Frequency);

            --  then frequency calculation and overflow check are run AGAIN
            --  immediately using this new value, but this second new frequency
            --  is not written back
            Calculate_New_Frequency (Channel, New_Frequency);
         end if;
      end if;
      if Channel.Sweep_Enabled then
         --  TODO: Line too long
         Start (Channel.Sweep_Timer, Actual_Effect_Periods (Channel.Sweep_Period));
      end if;
   end Step_Frequency_Sweep;

   procedure Tick_Frequency_Sweep (Channel : in out Sweeping_Square_Channel) is
      procedure Tick_Notify_Frequency_Sweep_Step is new Tick_Notify
        (Observer_Type => Sweeping_Square_Channel,
         Finished      => Step_Frequency_Sweep);
   begin
      Tick_Notify_Frequency_Sweep_Step (Channel.Sweep_Timer, Channel);
   end Tick_Frequency_Sweep;

   overriding
   procedure Trigger (Channel : in out Sweeping_Square_Channel) is
      New_Frequency : Integer;
      Sweep_Period  : Sweep_Period_Type renames Channel.Sweep_Period;
      Sweep_Shift   : Sweep_Shift_Type renames Channel.Sweep_Shift;
   begin
      Parent (Channel).Trigger;
      Channel.Sweep_Negated := False;

      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Frequency_Sweep
      --
      --  During a trigger event, several things occur:
      --  - Square 1's frequency is copied to the shadow register.
      Channel.Shadow_Frequency := Channel.Frequency_In.Frequency;
      --  - The sweep timer is reloaded.
      Channel.Sweep_Timer.Start (Actual_Effect_Periods (Channel.Sweep_Period));
      --  - The internal enabled flag is set if either the sweep period or shift
      --  are non-zero, cleared otherwise.
      Channel.Sweep_Enabled := Sweep_Period /= 0 or Sweep_Shift /= 0;
      --  - If the sweep shift is non-zero, frequency calculation and the
      --  overflow check are performed immediately.
      if Channel.Sweep_Shift /= 0 then
         Calculate_New_Frequency (Channel, New_Frequency);
      end if;
   end Trigger;

   overriding
   function Read_NRx0
     (Channel : Sweeping_Square_Channel) return Byte
   is
   begin
      return Channel.NRx0;
   end Read_NRx0;

   overriding
   procedure Write_NRx0
     (Channel : in out Sweeping_Square_Channel;
      Value   : Byte)
   is
      NRx0_In : constant NRx0_Frequency_Sweep_IO
        := To_NRx0_Frequency_Sweep_IO (Value);
   begin
      Channel.NRx0 := Value or NRx0_Sweep_Mask;
      Channel.Sweep_Period := NRx0_In.Period;
      Channel.Sweep_Negate := NRx0_In.Negate;
      Channel.Sweep_Shift  := NRx0_In.Shift;

--        Put_Line ("Period " & Channel.Sweep_Period'Img & " " &
--                  "Negate " & Channel.Sweep_Negate'Img & " " &
--                  "Negated " & Channel.Sweep_Negated'Img & " " &
--                    "Shift" & Channel.Sweep_Shift'Img  & " " &
--                      "SE " & Channel.Sweep_Enabled'Img);

      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Obscure_Behavior
      --
      --  Clearing the sweep negate mode bit in NR10 after at least one sweep
      --  calculation has been made using the negate mode since the last trigger
      --  causes the channel to be immediately disabled. This prevents you from
      --  having the sweep lower the frequency then raise the frequency without
      --  a trigger inbetween.
      if not Channel.Sweep_Negate and Channel.Sweep_Negated then
         --  Put_Line ("Sweep Negate based disable");
         Channel.Disable (Self_Disable);
      end if;
   end Write_NRx0;

   overriding
   function Id (Channel : Sweeping_Square_Channel) return Channel_Id is
      pragma Unreferenced (Channel);
   begin
      return NR1;
   end Id;

end Gade.Audio.Channels.Pulse.Square.Sweeping;
