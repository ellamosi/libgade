with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Gade.Audio.Square.Plain is

   procedure Reset (Ch : out Plain_Square_Channel) is
   begin
      Put_Line ("Base Reset");

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
      Ch.Duty := Half;

      --  TODO: Initialize all registers

      Ch.Read_Masks := Get_Read_Masks (Plain_Square_Channel'Class (Ch));
      Extended_Reset (Plain_Square_Channel'Class (Ch));
   end Reset;

   function Get_Read_Masks (Ch : Plain_Square_Channel) return Register_Masks is
      pragma Unreferenced (Ch);
   begin
      return Read_Masks;
   end Get_Read_Masks;

   procedure Read
     (Ch       : in out Plain_Square_Channel'Class;
      Register : Channel_Register;
      Value    : out Byte)
   is
   begin
      Value := Ch.IO.Space (Register) or Ch.Read_Masks (Register);
   end Read;

   procedure Write
     (Ch       : in out Plain_Square_Channel'Class;
      Register : Channel_Register;
      Value    : Byte)
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
         when NRx3 => -- FreqLo
            null;
            Put_Line ("Write Freq Low (Old) - V:" & Value'Img & " NRx3" & Ch.IO.Space (NRx3)'Img &
                        " NRx4" & Ch.IO.Space (NRx4)'Img & " Freq" & Ch.IO.Frequency'Img);
         when NRx4 =>
            --  Control/Trigger/FreqHi
            Trigger (Ch);
            Put_Line ("Write Freq High (Old) - V:" & Value'Img & " NRx3" & Ch.IO.Space (NRx3)'Img &
                        " NRx4" & Ch.IO.Space (NRx4)'Img & " Freq" & Ch.IO.Frequency'Img);
      end case;
   exception
      when E : others =>
         Put_Line ("Unexpected Square Channel Exception");
         Put_Line (Exception_Information (E));
   end Write;

   --  Writing a value to NRx4 with bit 7 set causes the following things to
   --  occur:
   --
   --  - Channel is enabled (see length counter).
   --  - If length counter is zero, it is set to 64 (256 for wave channel).
   --  - Frequency timer is reloaded with period.
   --  - Volume envelope timer is reloaded with period.
   --  - Channel volume is reloaded from NRx2.
   --  - Noise channel's LFSR bits are all set to 1.
   --  - Wave channel's position is set to 0 but sample buffer is NOT refilled.
   --  - Square 1's sweep does several things (see frequency sweep).
   --
   --  Note that if the channel's DAC is off, after the above actions occur the
   --  channel will be immediately disabled again.
   procedure Trigger (Ch : in out Plain_Square_Channel) is
   begin
      if Ch.IO.Trigger then
         Put_Line ("Legacy Trigger");
         Set_Frequency (Ch, Ch.IO.Frequency);
         Trigger_Volume_Envelope (Ch);
         Set_Volume (Ch, Ch.Volume_Envelope.Start_Volume);
         Trigger_Length (Ch);

         Ch.Enabled := True;

         Extended_Trigger (Plain_Square_Channel'Class (Ch));
      end if;
   end Trigger;

   procedure Reload_Length (Ch : in out Plain_Square_Channel) is
   begin
      Ch.Duty := Ch.IO.Duty;
      if Ch.IO.Length_Enable then
         Setup (Ch.Length_Timer, 64 - Natural (Ch.IO.Length_Load));
         Start (Ch.Length_Timer);
      else
         Stop (Ch.Length_Timer);
      end if;
   end Reload_Length;

   procedure Trigger_Volume_Envelope (Ch : in out Plain_Square_Channel) is
   begin
      Reset (Ch.Volume_Envelope.TMR);
   end Trigger_Volume_Envelope;

   procedure Trigger_Length (Ch : in out Plain_Square_Channel) is
   begin
      Reset (Ch.Length_Timer);
   end Trigger_Length;

   procedure Set_Volume_Envelope (Ch : in out Plain_Square_Channel) is
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

   procedure Set_Volume
     (Ch     : in out Plain_Square_Channel;
      Volume : Channel_Volume_Type)
   is
      Volume_Sample : constant Sample := Sample (Volume);
   begin
      Ch.Volume := Volume;
      Ch.Pulse_Levels := (-Volume_Sample, Volume_Sample);
   end Set_Volume;

   procedure Set_Frequency
     (Ch   : in out Plain_Square_Channel;
      Freq : Frequency_Type)
   is
      Period : constant Natural := 2048 - Natural (Freq);
      Hi_Mult : constant Natural := Hi_Duty_Sample_Multiplier (Ch.Duty) / 2;
      Lo_Mult : constant Natural := Lo_Duty_Sample_Multiplier (Ch.Duty) / 2;
   begin
      Ch.Pulse_Cycles :=
        (Pulse_Low  => Period * Lo_Mult,
         Pulse_High => Period * Hi_Mult);

      Put_Line ("Pulse Cycles Old:" & Ch.Pulse_Cycles (Pulse_Low)'Img & Ch.Pulse_Cycles (Pulse_High)'Img &
                " Freq" & Freq'Img);
   end Set_Frequency;

   procedure Disable (Ch : in out Plain_Square_Channel) is
   begin
      Ch.Enabled := False;
   end Disable;

   procedure Square_Step (Ch : in out Plain_Square_Channel; S : out Sample) is
   begin
      S := Ch.Level;
      if not Ch.Enabled then return; end if;
      Ch.Rem_Pulse_Cycles := Ch.Rem_Pulse_Cycles - 1;
      if Ch.Rem_Pulse_Cycles = 0 then
         Ch.Pulse_State := Next_Pulse_State (Ch.Pulse_State);
         Ch.Rem_Pulse_Cycles := Ch.Pulse_Cycles (Ch.Pulse_State);
         Ch.Level := Ch.Pulse_Levels (Ch.Pulse_State);
      end if;
   end Square_Step;

   procedure Envelope_Step (Ch : in out Plain_Square_Channel) is
      New_Volume : Channel_Volume_Type;
   begin
      Tick (Ch.Volume_Envelope.TMR);
      if Has_Finished (Ch.Volume_Envelope.TMR) then
         --  Trigger volume change, preserving pulse state
         --  Put_Line ("Ch.Volume" & Ch.Volume'Img & "Ch.Env_Step" & Ch.Volume_Envelope.Step'Img);
         New_Volume := Channel_Volume_Type (Integer (Ch.Volume) + Ch.Volume_Envelope.Step);
         Put_Line ("New_Volume" & New_Volume'Img);
         Set_Volume (Ch, New_Volume);

         if Ch.Volume = Channel_Max_Level or Ch.Volume = 0 then
            Stop (Ch.Volume_Envelope.TMR);
         else
            Reset (Ch.Volume_Envelope.TMR);
         end if;
      end if;
   end Envelope_Step;

   procedure Length_Step (Ch : in out Plain_Square_Channel) is
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

end Gade.Audio.Square.Plain;
