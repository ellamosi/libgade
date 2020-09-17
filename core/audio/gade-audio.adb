--  with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with System; use System;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Gade.Audio is

   procedure Create (Audio : aliased out Audio_Type) is
   begin
      null;
   end Create;

   procedure Reset (Audio : in out Audio_Type) is
   begin
      Put_Line ("Audio Reset");

      Audio.Elapsed_Cycles := 0;
      Audio.Frame_Seq_Step_Idx := 0;
      Audio.Rem_Frame_Seq_Ticks := Samples_Frame_Sequencer_Tick;

      Audio.Map.Square1.Trigger := False;
      Audio.Map.Square2.Trigger := False;

      Reset (Audio.Square1_State);
      Reset (Audio.Square2_State);
   end Reset;

   procedure Read
     (Audio   : in out Audio_Type;
      Address : Audio_IO_Address;
      Value   : out Byte)
   is
      pragma Unreferenced (Audio, Address);
   begin
      --  Put_Line ("R @" & Address'Img);
      Value := 0;
   end Read;

   procedure Write
     (Audio   : in out Audio_Type;
      Address : Audio_IO_Address;
      Value   : Byte)
   is
      System_Address : constant System.Address := Audio.Map.Space (Address)'Address;
   begin
      --  Put_Line ("AUDIO I/O");
      Audio.Map.Space (Address) := Value;
--        Put ("Audio IO: @");
--        Put (Integer (Address), Base => 16);
--        Put (" := ");
--        Put (Integer (Value), Base => 16);
--        New_Line;
      if Address = 16#FF10# then
         Put_Line ("SQ1 SW" & Audio.Map.Square1.Sweep_Period'Img & Audio.Map.Square1.Shift'Img & " WR" & Value'Img);
      elsif System_Address = Audio.Map.Square1.Trigger'Address and Audio.Map.Square1.Trigger then
         --  Put_Line ("SQ1 Trigger");
         Put_Line ("SQ1 Trigger f:" & Audio.Map.Square1.Frequency'Img &
                     " SW P:" & Audio.Map.Square1.Sweep_Period'Img &
                     " SW S:" & Audio.Map.Square1.Shift'Img);
         --  TODO: Combine these two methods?
         Trigger (Audio.Square1_State, Audio.Map.Square1.Frequency);
         Trigger_Frequency_Sweep
           (Audio.Square1_State,
            Audio.Map.Square1.Frequency,
            Audio.Map.Square1.Sweep_Period,
            Audio.Map.Square1.Negate,
            Audio.Map.Square1.Shift);
      elsif System_Address = Audio.Map.Square1.Length_Load'Address then
         Reload_Length
           (Audio.Square1_State,
            Audio.Map.Square1.Length_Enable,
            Audio.Map.Square1.Length_Load,
            Audio.Map.Square1.Duty);
         --  Put_Line ("SQ1 LL");
      elsif System_Address = Audio.Map.Square1.Volume'Address then
         Set_Volume_Envelope
           (Audio.Square1_State,
            Audio.Map.Square1.Volume,
            Audio.Map.Square1.Period,
            Audio.Map.Square1.Envelope_Direction);
         --  Put_Line ("SQ1 VE" & Audio.Map.Square1.Volume'Img &
         --  Audio.Map.Square1.Envelope_Direction'Img & Audio.Map.Square1.Period'Img);
      elsif System_Address = Audio.Map.Square2.Trigger'Address and Audio.Map.Square2.Trigger then
         --  Put_Line ("SQ2 Trigger");
         Trigger (Audio.Square2_State, Audio.Map.Square2.Frequency);
      elsif System_Address = Audio.Map.Square2.Length_Load'Address then
         Reload_Length
           (Audio.Square2_State,
            Audio.Map.Square2.Length_Enable,
            Audio.Map.Square2.Length_Load,
            Audio.Map.Square2.Duty);
         --  Put_Line ("SQ2 LL");
      elsif System_Address = Audio.Map.Square2.Volume'Address then
         Set_Volume_Envelope
           (Audio.Square2_State,
            Audio.Map.Square2.Volume,
            Audio.Map.Square2.Period,
            Audio.Map.Square2.Envelope_Direction);
         --  Put_Line ("SQ2 VE" & Audio.Map.Square2.Volume'Img &
         --  Audio.Map.Square2.Envelope_Direction'Img & Audio.Map.Square2.Period'Img);
      elsif System_Address = Audio.Map.Wave.Trigger'Address and Audio.Map.Wave.Trigger then
         null;
         --  Put_Line ("WAV Trigger");
      end if;
   exception
      when others =>
         Put_Line ("Exception");
   end Write;

   procedure Setup (T : out Timer; Ticks : Positive := 1) is
   begin
      T.Initial := Ticks;
      Stop (T);
   end Setup;

   procedure Start (T : in out Timer) is
   begin
      T.Tick := 1;
   end Start;

   procedure Reset (T : in out Timer) is
   begin
      T.Remaining := T.Initial;
   end Reset;

   procedure Stop (T : in out Timer) is
   begin
      --  Reset timer, that way we can transparently tick a stopped timer that
      --  had finished.
      Reset (T);
      T.Tick := 0;
   end Stop;

   procedure Tick (T : in out Timer) is
   begin
      T.Remaining := T.Remaining - T.Tick;
   end Tick;

   function Has_Finished (T : Timer) return Boolean is
   begin
      return T.Remaining = 0;
   end Has_Finished;

   procedure Reset (Ch : out Square_Channel_State) is
   begin
      Ch.Pulse_State := Pulse_Low;
      Ch.Level := 0;
      Ch.Pulse_Levels := (0, 0);
      Ch.Rem_Pulse_Cycles := 1;
      Ch.Pulse_Cycles := (1, 1);
      Setup (Ch.Envelope_Timer);
      Ch.Env_Step := 0;
      Setup (Ch.Length_Timer);
      Ch.Volume := 0;
      Ch.Env_Start_Volume := 0;
      Ch.Enabled := False;
      Ch.Duty := Half;
   end Reset;

   overriding
   procedure Reset (Ch : out Frequency_Sweep_Channel_State) is
   begin
      Square_Channel_State (Ch).Reset;
      Ch.Sweep_Negate := False;
      Setup (Ch.Sweep_Timer);
   end Reset;

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
   procedure Trigger
     (Ch        : in out Square_Channel_State;
      Frequency : Frequency_Type)
   is
   begin
      Set_Frequency (Ch, Frequency);
      Trigger_Volume_Envelope (Ch);
      Set_Volume (Ch, Ch.Env_Start_Volume);
      Trigger_Length (Ch);

      Ch.Enabled := True;
   end Trigger;

   procedure Reload_Length
     (Ch     : in out Square_Channel_State;
      Enable : Boolean;
      Load   : Length_Load_Type; -- Up to 256 for wave
      Duty   : Duty_Type)
   is
   begin
      Ch.Duty := Duty;
      if Enable then
         Setup (Ch.Length_Timer, 64 - Natural (Load));
         Start (Ch.Length_Timer);
      else
         Stop (Ch.Length_Timer);
      end if;
   end Reload_Length;

   procedure Trigger_Volume_Envelope (Ch : in out Square_Channel_State) is
   begin
      Reset (Ch.Envelope_Timer);
   end Trigger_Volume_Envelope;

   procedure Trigger_Length (Ch : in out Square_Channel_State) is
   begin
      Reset (Ch.Length_Timer);
   end Trigger_Length;

   --  During a trigger event, several things occur:
   --  - Square 1's frequency is copied to the shadow register.
   --  - The sweep timer is reloaded.
   --  - The internal enabled flag is set if either the sweep period or shift
   --    are non-zero, cleared otherwise.
   --  - If the sweep shift is non-zero, frequency calculation and the overflow
   --    check are performed immediately.
   procedure Trigger_Frequency_Sweep
     (Ch        : in out Frequency_Sweep_Channel_State;
      Frequency : Frequency_Type;
      Period    : Sweep_Period_Type;
      Negate    : Boolean;
      Shift     : Sweep_Shift_Type)
   is
   begin
      if Period > 0 and Shift > 0 then
         Ch.Sweep_Shift := Shift;
         --  Enable timer
         Setup (Ch.Sweep_Timer, Natural (Period));
         Start (Ch.Sweep_Timer);

         Ch.Shadow_Frequency := Frequency;
         Ch.Sweep_Negate := Negate;

         --  if Shift /= 0 then Frequency_Sweep_Step (Ch); end if;
      else
         Stop (Ch.Sweep_Timer);
      end if;
   end Trigger_Frequency_Sweep;

   procedure Set_Volume_Envelope
     (Ch        : in out Square_Channel_State;
      Volume    : Channel_Volume_Type;
      Period    : Period_Type;
      Direction : Envelope_Direction_Type)
   is
   begin
      Ch.Env_Start_Volume := Volume;

      if Period > 0 then
         Setup (Ch.Envelope_Timer, Positive (Period));

         --  TODO: Make this more readable, shouldn't nest
         if ((Volume /= 0 and Direction = Down) or
             (Volume /= Channel_Max_Level and Direction = Up))
         then
            Start (Ch.Envelope_Timer);
            Ch.Env_Step := Envelope_Steps (Direction);
         end if;
      else
         Stop (Ch.Envelope_Timer);
      end if;
   end Set_Volume_Envelope;

   procedure Set_Volume
     (Ch     : in out Square_Channel_State;
      Volume : Channel_Volume_Type)
   is
      Volume_Sample : constant Sample := Sample (Volume);
   begin
      Ch.Volume := Volume;
      Ch.Pulse_Levels := (-Volume_Sample, Volume_Sample);
   end Set_Volume;

   procedure Set_Frequency
     (Ch   : in out Square_Channel_State;
      Freq : Frequency_Type)
   is
      Period : constant Natural := 2048 - Natural (Freq);
      Hi_Mult : constant Natural := Hi_Duty_Sample_Multiplier (Ch.Duty) / 2;
      Lo_Mult : constant Natural := Lo_Duty_Sample_Multiplier (Ch.Duty) / 2;
   begin
      Ch.Pulse_Cycles :=
        (Pulse_Low  => Period * Lo_Mult,
         Pulse_High => Period * Hi_Mult);
   end Set_Frequency;

   procedure Square_Step (Ch : in out Square_Channel_State; S : out Sample) is
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

   procedure Envelope_Step (Ch : in out Square_Channel_State) is
      New_Volume : Channel_Volume_Type;
   begin
      Tick (Ch.Envelope_Timer);
      if Has_Finished (Ch.Envelope_Timer) then
         --  Trigger volume change, preserving pulse state
         Put_Line ("Ch.Volume" & Ch.Volume'Img & "Ch.Env_Step" & Ch.Env_Step'Img);
         New_Volume := Channel_Volume_Type (Integer (Ch.Volume) + Ch.Env_Step);
         Put_Line ("New_Volume" & New_Volume'Img);
         Set_Volume (Ch, New_Volume);

         if Ch.Volume = Channel_Max_Level or Ch.Volume = 0 then
            Stop (Ch.Envelope_Timer);
         else
            Reset (Ch.Envelope_Timer);
         end if;
      end if;
   end Envelope_Step;

   procedure Length_Step (Ch : in out Square_Channel_State) is
   begin
      Tick (Ch.Length_Timer);
      if Has_Finished (Ch.Length_Timer) then
         Set_Volume (Ch, 0);
         Ch.Level := Ch.Pulse_Levels (Ch.Pulse_State);

         Stop (Ch.Length_Timer);
         Stop (Ch.Envelope_Timer); -- TODO: Maybe stop sweep too?
      end if;
   end Length_Step;

   procedure Frequency_Sweep_Shift (Ch : in out Frequency_Sweep_Channel_State) is
      New_Freq : Frequency_Type;
      Shifted : Integer;
      Out_Of_Range : Boolean;
   begin
      --  Frequency calculation consists of taking the value in the frequency
      --  shadow register, shifting it right by sweep shift, optionally
      --  negating the value, and summing this with the frequency shadow
      --  register to produce a new frequency.
      Shifted := (Integer (Ch.Shadow_Frequency) / 2 ** Natural (Ch.Sweep_Shift));
      if Ch.Sweep_Negate then
         --  Down
         New_Freq := Ch.Shadow_Frequency - Frequency_Type (Shifted);
         Out_Of_Range := New_Freq > Ch.Shadow_Frequency;
      else
         --  Up
         New_Freq := Ch.Shadow_Frequency + Frequency_Type (Shifted);
         Out_Of_Range := New_Freq < Ch.Shadow_Frequency;
      end if;

      if Out_Of_Range then
         Stop (Ch.Sweep_Timer);
         Ch.Enabled := False;
      else
         Ch.Shadow_Frequency := New_Freq;
         Reset (Ch.Sweep_Timer);
         Put_Line ("Sweep New Freq (Dec):" & New_Freq'Img);
         Set_Frequency (Ch, New_Freq);
         --  TODO: Update register
      end if;
   end Frequency_Sweep_Shift;

   procedure Frequency_Sweep_Step (Ch : in out Frequency_Sweep_Channel_State) is
   begin
      Tick (Ch.Sweep_Timer);
      if Has_Finished (Ch.Sweep_Timer) then Frequency_Sweep_Shift (Ch); end if;
   end Frequency_Sweep_Step;

   procedure Tick_Frame_Sequencer (Audio : in out Audio_Type) is
   begin
      Audio.Rem_Frame_Seq_Ticks := Audio.Rem_Frame_Seq_Ticks - 1;
      if Audio.Rem_Frame_Seq_Ticks = 0 then
         Audio.Frame_Seq_Step_Idx := Audio.Frame_Seq_Step_Idx + 1;
         case Frame_Sequencer_Steps (Audio.Frame_Seq_Step_Idx) is
            when Length_Counter =>
               Length_Step (Audio.Square1_State);
               Length_Step (Audio.Square2_State);
            when Length_Counter_Frequency_Sweep =>
               Length_Step (Audio.Square1_State);
               Length_Step (Audio.Square2_State);
               Frequency_Sweep_Step (Audio.Square1_State);
            when Volume_Envelope => null;
               Envelope_Step (Audio.Square1_State);
               Envelope_Step (Audio.Square2_State);
            when None => null;
         end case;
         Audio.Rem_Frame_Seq_Ticks := Samples_Frame_Sequencer_Tick;
      end if;
   end Tick_Frame_Sequencer;

   procedure Report_Cycles
     (Audio        : in out Audio_Type;
      Audio_Buffer : Audio_Buffer_Access;
      Cycles       : Positive)
   is
      Target_Cycles : constant Natural := Audio.Elapsed_Cycles + Cycles / 4;


      S1, S2, S_Out : Sample;
      --  Last_Index : constant Natural := Natural (Cycles) / 4 - 1;
   begin
      while Audio.Elapsed_Cycles < Target_Cycles loop
         Square_Step (Audio.Square1_State, S1);
         S2 := 0;
         Square_Step (Audio.Square2_State, S2);

         Tick_Frame_Sequencer (Audio);

         --  Put_Line (S2'Img);
         S_Out := (S1 + S2) * 8 * 32; -- Temporary master volume and dyn range adjustment
         Audio_Buffer (Audio.Elapsed_Cycles) := (S_Out, S_Out);
         Audio.Elapsed_Cycles := Audio.Elapsed_Cycles + 1;
      end loop;
   exception
      when E : others => Put_Line (Exception_Information (E));
   end Report_Cycles;

   procedure Flush_Frame
     (Audio        : in out Audio_Type;
      Audio_Buffer : Audio_Buffer_Access;
      Cycles       : Positive)
   is
      pragma Unreferenced (Cycles, Audio_Buffer);
   begin
      null;
      --  Report_Cycles (Audio, GB, Audio_Buffer, Cycles);
      --  Put_Line (Audio.Elapsed_Cycles'Img & " fs");
      Audio.Elapsed_Cycles := 0;
   end Flush_Frame;

end Gade.Audio;
