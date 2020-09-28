--   with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Audio.Noise is

   procedure Reset (Ch : out Noise_Channel) is
   begin
      Ch.Pulse_State := Pulse_Low;
      Ch.Level := 0;
      Ch.Pulse_Levels := (0, 0);
      Ch.Rem_Pulse_Cycles := 1;
      Ch.Pulse_Cycles := (1, 1);
      --  Volume Envelope
      Setup (Ch.Volume_Envelope.TMR);
      Ch.Volume_Envelope.Step := 0;
      Ch.Volume_Envelope.Start_Volume := 0;

      Setup (Ch.Length_Timer);
      Ch.Volume := 0;
      Ch.Enabled := False;

      --  TODO: Initialize all registers

      --  Noise specific
      Ch.Divisor := 1;
      Ch.LFSR := 0;
   end Reset;

   procedure Read
     (Ch    : in out Noise_Channel'Class; Register : Channel_Register;
      Value :    out Byte)
   is
   begin
      Value := Ch.IO.Space (Register) or Read_Masks (Register);
   end Read;

   procedure Write
     (Ch    : in out Noise_Channel'Class; Register : Channel_Register;
      Value :        Byte)
   is
   begin
      Ch.IO.Space (Register) := Value;
      --  TODO: Should probably only set things upon trigger
      case Register is
         when NRx0 => -- Unused
            null;
         when NRx1 => -- Duty/LengthLoad
            --  Counter can be reloaded at any time
            Reload_Length (Ch);
         when NRx2 =>
            --  Volume
            --  Writing to NRx2 causes obscure effects on the volume that differ
            --  on different Game Boy models (see obscure behavior).
            Set_Volume_Envelope (Ch);
         when NRx3 =>
            --  Noise
            Set_Noise (Ch);
         when NRx4 =>
            --  Control/Trigger/FreqHi
            Trigger (Ch);
      end case;
   end Write;

   procedure Trigger (Ch : in out Noise_Channel) is
   begin
      if Ch.IO.Trigger then
         --  Duty would be set within Set Frequency
         --  Set_Frequency (Ch, Ch.IO.Frequency);
         Trigger_Volume_Envelope (Ch);
         Set_Volume (Ch, Ch.Volume_Envelope.Start_Volume);
         Trigger_Length (Ch);

         Ch.Enabled := True;

         --  Noise Specific
         Ch.LFSR := 16#7FFF#;
      end if;
   end Trigger;

   procedure Trigger_Volume_Envelope (Ch : in out Noise_Channel) is
   begin
      Reset (Ch.Volume_Envelope.TMR);
   end Trigger_Volume_Envelope;

   procedure Trigger_Length (Ch : in out Noise_Channel) is
   begin
      Reset (Ch.Length_Timer);
   end Trigger_Length;

   procedure Square_Step (Ch : in out Noise_Channel; S : out Sample) is
      Low_0, Low_1, XORed : Shift_Register;
      R : Sample;
   begin
      S := Ch.Level;
      if not Ch.Enabled then return; end if;
      --  Ch.Rem_Pulse_Cycles := Ch.Rem_Pulse_Cycles - 1;
--        if Ch.Rem_Pulse_Cycles = 0 then
--           Ch.Pulse_State := Next_Pulse_State (Ch.Pulse_State);
--           Ch.Rem_Pulse_Cycles := Ch.Pulse_Cycles (Ch.Pulse_State);
--           Ch.Level := Ch.Pulse_Levels (Ch.Pulse_State);
--        end if;
      Ch.Rem_Pulse_Cycles := Ch.Rem_Pulse_Cycles - 1;
      if Ch.Rem_Pulse_Cycles = 0 then
         Ch.Pulse_State := Next_Pulse_State (Ch.Pulse_State);
         Ch.Rem_Pulse_Cycles := Ch.Divisor * 16; -- ???

         Low_0 := Ch.LFSR and 16#0001#;
         Ch.LFSR := Ch.LFSR / 2;
         Low_1 := Ch.LFSR and 16#0001#;
         XORed := Low_0 xor Low_1;
         Ch.LFSR := Ch.LFSR or (XORed * 2 ** 14);
         if Ch.IO.LFSR_Width_Mode then
            Ch.LFSR := (Ch.LFSR and 16#7FBF#) or (XORed * 2 ** 6);
         end if;
         R := Sample ((not Ch.LFSR) and 16#0001#);
         --  Ch.Level := Ch.Pulse_Levels (Ch.Pulse_State) * R;
         Ch.Level := Sample (Ch.Volume) * R;
         --  Put_Line ("LFSR:" & Ch.LFSR'Img & " R:" & R'Img);
      end if;
   end Square_Step;

   --  The linear feedback shift register (LFSR) generates a pseudo-random bit
   --  sequence. It has a 15-bit shift register with feedback. When clocked by
   --  the frequency timer, the low two bits (0 and 1) are XORed, all bits are
   --  shifted right by one, and the result of the XOR is put into the now-empty
   --  high bit. If width mode is 1 (NR43), the XOR result is ALSO put into bit
   --  6 AFTER the shift, resulting in a 7-bit LFSR. The waveform output is bit
   --  0 of the LFSR, INVERTED.

   procedure Envelope_Step (Ch : in out Noise_Channel) is
      New_Volume : Channel_Volume_Type;
   begin
      Tick (Ch.Volume_Envelope.TMR);
      if Has_Finished (Ch.Volume_Envelope.TMR) then
         --  Trigger volume change, preserving pulse state
         --  Put_Line ("Ch.Volume" & Ch.Volume'Img & "Ch.Env_Step" & Ch.Volume_Envelope.Step'Img);
         New_Volume := Channel_Volume_Type (Integer (Ch.Volume) + Ch.Volume_Envelope.Step);
         --  Put_Line ("New_Volume" & New_Volume'Img);
         Set_Volume (Ch, New_Volume);

         if Ch.Volume = Channel_Max_Level or Ch.Volume = 0 then
            Stop (Ch.Volume_Envelope.TMR);
         else
            Reset (Ch.Volume_Envelope.TMR);
         end if;
      end if;
   end Envelope_Step;

   procedure Length_Step (Ch : in out Noise_Channel) is
   begin
      Tick (Ch.Length_Timer);
      if Has_Finished (Ch.Length_Timer) then
         Set_Volume (Ch, 0);
         Ch.Level := Ch.Pulse_Levels (Ch.Pulse_State);

         Stop (Ch.Length_Timer);
         Stop (Ch.Volume_Envelope.TMR);
         --  TODO: Maybe stop sweep too?
      end if;
   end Length_Step;

   procedure Set_Volume
     (Ch : in out Noise_Channel; Volume : Channel_Volume_Type)
   is
      Volume_Sample : constant Sample := Sample (Volume);
   begin
      Ch.Volume := Volume;
      Ch.Pulse_Levels := (-Volume_Sample, Volume_Sample);
   end Set_Volume;

   procedure Set_Volume_Envelope (Ch : in out Noise_Channel) is
      Volume    : constant Channel_Volume_Type := Ch.IO.Volume;
      Period    : constant Period_Type := Ch.IO.Period;
      Direction : constant Envelope_Direction_Type := Ch.IO.Envelope_Direction;
   begin
      Ch.Volume_Envelope.Start_Volume := Volume;

      if Period > 0 then
         Setup (Ch.Volume_Envelope.TMR, Positive (Period));

         --  TODO: Make this more readable, shouldn't nest
         if ((Volume /= 0 and Direction = Down) or
             (Volume /= Channel_Max_Level and Direction = Up))
         then
            Start (Ch.Volume_Envelope.TMR);
            Ch.Volume_Envelope.Step := Envelope_Steps (Direction);
         end if;
      else
         Stop (Ch.Volume_Envelope.TMR);
      end if;
   end Set_Volume_Envelope;

   procedure Reload_Length (Ch : in out Noise_Channel) is
   begin
      if Ch.IO.Length_Enable then
         Setup (Ch.Length_Timer, 64 - Natural (Ch.IO.Length_Load));
         Start (Ch.Length_Timer);
      else
         Stop (Ch.Length_Timer);
      end if;
   end Reload_Length;

   procedure Set_Noise (Ch : in out Noise_Channel) is
   begin
      Ch.Divisor := Divisor (Ch.IO.Divisor_Code);
   end Set_Noise;

end Gade.Audio.Noise;
