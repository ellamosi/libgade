with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Audio.Channels.Pulse.Square.Sweeping is

   overriding procedure Reset (Channel : out Sweeping_Square_Channel) is
   begin
      Parent (Channel).Reset;
      Channel.Sweep_Enabled := False;
      Channel.NRx0 := NRx0_Sweep_Mask;
      Channel.Sweep_Negate := False;
      Channel.Sweep_Negated := False;
      Setup (Channel.Sweep_Timer);
      Channel.Sweep_Period := 0;
      Channel.Sweep_Shift := 0;
   end Reset;

   overriding
   procedure Disable
     (Channel : in out Sweeping_Square_Channel;
      Mode    : Disable_Mode)
   is
   begin
      Parent (Channel).Disable (Mode);
      Channel.Sweep_Timer.Pause;
      Channel.Sweep_Enabled := False;
      if Mode = APU_Power_Off then
         Channel.NRx0 := NRx0_Sweep_Mask;
         Channel.Sweep_Negate := False;
         Channel.Sweep_Negated := False;
         Channel.Sweep_Period := 0;
         Channel.Sweep_Shift := 0;
      end if;
   end Disable;

   --  TODO: In spec
   procedure Calculate_New_Frequency_Handle_Overflow
     (Channel       : in out Sweeping_Square_Channel;
      New_Frequency : out Integer);
   procedure Calculate_New_Frequency_Handle_Overflow
     (Channel       : in out Sweeping_Square_Channel;
      New_Frequency : out Integer)
   is
      Shifted  : Integer;
   begin
      --  Shifted := Shift_Right_Arithmetic (Unsigned_16 (Channel.Shadow_Frequency), Natural (Channel.Sweep_Shift));
      Shifted := (Integer (Channel.Shadow_Frequency) / 2 ** Natural (Channel.Sweep_Shift));
      if Channel.Sweep_Negate then
         --  Down
--           if Channel.Shadow_Frequency > 16#3FF# then
--              Old := Integer (Channel.Shadow_Frequency) - 16#800#;
--              Shifted := (Old / 2 ** Natural (Channel.Sweep_Shift));
--           end if;

         Put_Line ("Shifted" & Shifted'Img);

         Channel.Sweep_Negated := True;

--           if Channel.Shadow_Frequency > 16#3FF# then
--              New_Frequency := Natural (Channel.Shadow_Frequency) - 16#800# - Shifted;
--           else
--              New_Frequency := Natural (Channel.Shadow_Frequency) - Shifted;
--           end if;
         New_Frequency := Natural (Channel.Shadow_Frequency) - Shifted;
--           if New_Frequency < 0 then
--              New_Frequency := 16#800# + New_Frequency;
--           end if;


         --  New_Freq := Channel.Shadow_Frequency - Frequency_Type (Shifted);
         --  Out_Of_Range := New_Freq > Channel.Shadow_Frequency;
      else
         --  Up
         Put_Line ("Shifted" & Shifted'Img);
         New_Frequency := Natural (Channel.Shadow_Frequency) + Shifted;
         --  Out_Of_Range := New_Freq < Channel.Shadow_Frequency;
      end if;

      Put_Line ("fCalc - Old f:" & Channel.Shadow_Frequency'Img &
                  " New f:" & New_Frequency'Img &
                  " Negated: " & Channel.Sweep_Negate'Img);

      if New_Frequency > Natural (Frequency_Type'Last) then
         Channel.Disable (Self_Disable);
         Channel.Sweep_Enabled := False;
      end if;
   end Calculate_New_Frequency_Handle_Overflow;


   --  TODO: In spec
   procedure Calculate_New_Frequency (Channel : in out Sweeping_Square_Channel);
   procedure Calculate_New_Frequency
     (Channel : in out Sweeping_Square_Channel)
   is
      New_Frequency : Integer;
   begin
      --  Frequency calculation consists of taking the value in the frequency
      --  shadow register, shifting it right by sweep shift, optionally
      --  negating the value, and summing this with the frequency shadow
      --  register to produce a new frequency.
      Calculate_New_Frequency_Handle_Overflow (Channel, New_Frequency);
      --  Out_Of_Range := New_Freq > Frequency_Type'Last

      if New_Frequency <= 2047 and Channel.Sweep_Shift /= 0 then
         Channel.Shadow_Frequency := Frequency_Type (New_Frequency);
         --  Hack: Fix frequency reading
         Channel.Frequency_In.Frequency := Channel.Shadow_Frequency;
         Channel.Set_Frequency (Channel.Shadow_Frequency);
         --  Run calculation AGAIN and do overflow check, but discard new
         --  frequency.
         Put_Line ("Calculating frequency AGAIN");
         Calculate_New_Frequency_Handle_Overflow (Channel, New_Frequency);
      end if;
   end Calculate_New_Frequency;

   procedure Step_Frequency_Sweep (Channel : in out Sweeping_Square_Channel) is
   begin
      Put_Line ("Step_Frequency_Sweep (SE: " & Channel.Sweep_Enabled'Img &
                " Sweep_Period: " & Channel.Sweep_Period'Img & ")");
      --  Experiment
      if Channel.Sweep_Enabled and Channel.Sweep_Period > 0 then
         Calculate_New_Frequency (Channel);
      end if;
      if Channel.Sweep_Enabled then
         Start (Channel.Sweep_Timer, Actual_Effect_Periods (Channel.Sweep_Period));
         Put_Line ("Scheduling next step in ticks:" & Channel.Sweep_Timer.Ticks_Remaining'Img);
      end if;
   end Step_Frequency_Sweep;

   procedure Tick_Frequency_Sweep (Channel : in out Sweeping_Square_Channel) is
      procedure Tick_Notify_Frequency_Sweep_Step is new Tick_Notify_Repeatable
        (Observer_Type => Sweeping_Square_Channel,
         Finished      => Step_Frequency_Sweep);
   begin
--        Put_Line ("Tick_Frequency_Sweep - Rem:" & Channel.Sweep_Timer.Ticks_Remaining'Img &
--                    " Enabled: " & Channel.Sweep_Timer.Enabled'Img);
      Tick_Notify_Frequency_Sweep_Step (Channel.Sweep_Timer, Channel);
   end Tick_Frequency_Sweep;

   overriding
   procedure Trigger (Channel : in out Sweeping_Square_Channel) is
      F : Integer;
   begin
      --  Put_Line ("TRIGGER");
      Parent (Channel).Trigger;
      Channel.Shadow_Frequency := Channel.Frequency_In.Frequency; -- TODO: save frequency in component

      Channel.Sweep_Enabled := Channel.Sweep_Period > 0 or Channel.Sweep_Shift > 0;
      Channel.Sweep_Negated := False;

      Put_Line ("Sweep Trigger -- Period:" & Channel.Sweep_Period'Img &
                  " Shift:" & Channel.Sweep_Shift'Img &
                  " iFreq:" & Channel.Frequency_In.Frequency'Img &
                  " SE: " & Channel.Sweep_Enabled'Img);

      --  if Channel.Sweep_Period > 0 then

      Channel.Sweep_Timer.Start (Actual_Effect_Periods (Channel.Sweep_Period));
      Put_Line ("Setting up timer, rem:" & Channel.Sweep_Timer.Ticks_Remaining'Img);
      --  end if;

      if Channel.Sweep_Shift > 0 then
         --  Calculate_New_Frequency (Channel);
         Put_Line ("Stepin");
         Calculate_New_Frequency_Handle_Overflow (Channel, F);
         --  Calculate_New_Frequency (Channel);
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
      --  F : Integer;
   begin
      Channel.NRx0 := Value or NRx0_Sweep_Mask;
      Channel.Sweep_Period := NRx0_In.Period;
      Channel.Sweep_Negate := NRx0_In.Negate;
      Channel.Sweep_Shift  := NRx0_In.Shift;

      --  Breaks basic sweep test
      --  Channel.Sweep_Enabled := Channel.Sweep_Period > 0 and Channel.Sweep_Shift > 0;

      Put_Line ("Period " & Channel.Sweep_Period'Img & " " &
                "Negate " & Channel.Sweep_Negate'Img & " " &
                  "Shift" & Channel.Sweep_Shift'Img  & " " &
                    "SE " & Channel.Sweep_Enabled'Img);

      if not Channel.Sweep_Negate and Channel.Sweep_Negated then
         --  Clearing the sweep negate mode bit in NR10 after at least one sweep
         --  calculation has been made using the negate mode since the last
         --  trigger causes the channel to be immediately disabled. This
         --  prevents you from having the sweep lower the frequency then raise
         --  the frequency without a trigger inbetween.
         Put_Line ("Sweep Negate based disable");
         Channel.Disable (Self_Disable);
      end if;
      --  @enabled = false if !@negate &&
      --  if Channel.Sweep_Period > 0 then
      --  Calculate_New_Frequency_Handle_Overflow (Channel, F);
--        if Channel.Sweep_Enabled then
--           Channel.Sweep_Timer.Start (Actual_Effect_Periods (Channel.Sweep_Period));
--           Put_Line ("Setting up timer, rem:" & Channel.Sweep_Timer.Ticks_Remaining'Img);
--        end if;
      --  end if;
   end Write_NRx0;

   overriding
   function Name (Channel : Sweeping_Square_Channel) return String is
      pragma Unreferenced (Channel);
   begin
      return "Square 1";
   end Name;

end Gade.Audio.Channels.Pulse.Square.Sweeping;
