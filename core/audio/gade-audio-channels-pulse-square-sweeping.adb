with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Audio.Channels.Pulse.Square.Sweeping is

   overriding procedure Reset (Channel : out Sweeping_Square_Channel) is
   begin
      Square_Channel (Channel).Reset;
      Channel.NRx0 := 16#00#;
      Channel.Sweep_Negate := False;
      Setup (Channel.Sweep_Timer);
   end Reset;

   procedure Frequency_Sweep_Shift (Channel : in out Sweeping_Square_Channel) is
      New_Freq : Frequency_Type;
      Shifted : Integer;
      Out_Of_Range : Boolean;
   begin
      --  Frequency calculation consists of taking the value in the frequency
      --  shadow register, shifting it right by sweep shift, optionally
      --  negating the value, and summing this with the frequency shadow
      --  register to produce a new frequency.
      Shifted := (Integer (Channel.Shadow_Frequency) / 2 ** Natural (Channel.Sweep_Shift));
      if Channel.Sweep_Negate then
         --  Down
         New_Freq := Channel.Shadow_Frequency - Frequency_Type (Shifted);
         Out_Of_Range := New_Freq > Channel.Shadow_Frequency;
      else
         --  Up
         New_Freq := Channel.Shadow_Frequency + Frequency_Type (Shifted);
         Out_Of_Range := New_Freq < Channel.Shadow_Frequency;
      end if;

      if Out_Of_Range then
         Put_Line ("Freq Sweep Stopped");
         Channel.Disable;
      else
         Channel.Shadow_Frequency := New_Freq;
         Reset (Channel.Sweep_Timer);
         Put_Line ("Sweep New Freq:" & New_Freq'Img);
         Channel.Set_Frequency (New_Freq);
      end if;
   end Frequency_Sweep_Shift;

   procedure Frequency_Sweep_Step (Channel : in out Sweeping_Square_Channel) is
   begin
      Tick (Channel.Sweep_Timer);
      if Has_Finished (Channel.Sweep_Timer) then
         Frequency_Sweep_Shift (Channel);
      end if;
   end Frequency_Sweep_Step;

   overriding
   procedure Trigger (Channel : in out Sweeping_Square_Channel) is
   begin
      Put_Line ("TRIGGER");
      Square_Channel (Channel).Trigger;
      if Channel.Sweep_Shift > 0 then
         Put_Line ("Sweep Triggered");
         Setup (Channel.Sweep_Timer, Channel.Sweep_Period);
         Start (Channel.Sweep_Timer);

         Channel.Shadow_Frequency := Channel.Frequency_In.Frequency; -- TODO: save frequency in component

         --  if Shift /= 0 then Frequency_Sweep_Step (Ch); end if; ??????
      else
         Stop (Channel.Sweep_Timer);
      end if;
   end Trigger;

   overriding
   procedure Disable (Channel : in out Sweeping_Square_Channel) is
   begin
      Square_Channel (Channel).Disable;
      Stop (Channel.Sweep_Timer);
   end Disable;

   overriding
   function Read_NRx0
     (Channel : Sweeping_Square_Channel) return Byte
   is
   begin
      return Channel.NRx1;
   end Read_NRx0;

   overriding
   procedure Write_NRx0
     (Channel : in out Sweeping_Square_Channel;
      Value   : Byte)
   is
      NRx0_In : constant NRx0_Frequency_Sweep_IO
        := To_NRx0_Frequency_Sweep_IO (Value);
   begin
      Channel.NRx1 := Value or NRx0_Sweep_Mask;
      Channel.Sweep_Period := Actual_Effect_Periods (NRx0_In.Period);
      Channel.Sweep_Negate := NRx0_In.Negate;
      Channel.Sweep_Shift  := NRx0_In.Shift;
      Put_Line ("Write_NRx0 (Sweep):" & Value'Img);
      Put_Line ("Period " & Channel.Sweep_Period'Img & " " &
                "Negate " & Channel.Sweep_Negate'Img & " " &
                "Shift" & Channel.Sweep_Shift'Img);
   end Write_NRx0;

end Gade.Audio.Channels.Pulse.Square.Sweeping;
