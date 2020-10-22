with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Gade.Audio.Channels; use Gade.Audio.Channels;
with Gade.Audio.Channels.Pulse; use Gade.Audio.Channels.Pulse;
with Gade.Audio.Channels.Pulse.Noise; use Gade.Audio.Channels.Pulse.Noise;
with Gade.Audio.Channels.Pulse.Square; use Gade.Audio.Channels.Pulse.Square;
with Gade.Audio.Channels.Wave; use Gade.Audio.Channels.Wave;

with Gade.Audio.Channels.Pulse.Square.Sweeping;
use Gade.Audio.Channels.Pulse.Square.Sweeping;

package body Gade.Audio is

   function To_Channel_Register
     (Address : Audio_IO_Address)
      return Channel_Register;

   type Opaque_Audio_Type is record
      Square_1 : Sweeping_Square_Channel;
      Square_2 : Square_Channel;
      Wave     : Wave_Channel;
      Noise    : Noise_Channel;

      Output_Control : Channel_Output_Control;
      Volume_Control : Output_Volume_Control;
      Power_Control  : Power_Control_Status;

      Elapsed_Cycles   : Natural;
      Frame_Seq_Step_Idx : Frame_Sequencer_Step_Index;
      Rem_Frame_Seq_Ticks : Natural;
      Powered : Boolean;
      Frame_Seq_Step : Natural;
   end record;

   procedure Create (Audio : aliased out Audio_Type) is
   begin
      Audio := new Opaque_Audio_Type;
      Create (Audio.Square_1, Audio);
      Create (Audio.Square_2, Audio);
      Create (Audio.Wave, Audio);
      Create (Audio.Noise, Audio);
   end Create;

   procedure Reset (Audio : in out Audio_Type) is
   begin
      Put_Line ("Audio Reset");

      Audio.Elapsed_Cycles := 0;
      Audio.Frame_Seq_Step_Idx := 0;
      Audio.Frame_Seq_Step := 1;
      Audio.Rem_Frame_Seq_Ticks := Samples_Frame_Sequencer_Tick;

      Reset (Audio.Square_1);
      Reset (Audio.Square_2);
      Reset (Audio.Wave);
      Reset (Audio.Noise);

      Audio.Powered := True;

      Audio.Power_Control.Space := Power_Control_Status_Write_Mask;
   end Reset;

   function To_Channel_Register
     (Address : Audio_IO_Address)
      return Channel_Register
   is
      Rebased_Address : constant Word := Address - Audio_IO_Address'First;
      Register_Index : Word;
   begin
      Register_Index := Rebased_Address mod Channel_Register_Count;
      return Channel_Register'Val (Register_Index);
   end To_Channel_Register;

   procedure Read
     (Audio   : in out Audio_Type;
      Address : Audio_IO_Address;
      Value   : out Byte)
   is
   begin
      Value :=
        (case Address is
            when NR1_IO_Address =>
              Audio.Square_1.Read (To_Channel_Register (Address)),
            when NR2_IO_Address =>
              Audio.Square_2.Read (To_Channel_Register (Address)),
            when NR3_IO_Address =>
              Audio.Wave.Read (To_Channel_Register (Address)),
            when NR4_IO_Address =>
              Audio.Noise.Read (To_Channel_Register (Address)),
            when Output_Volume_Control_IO_Address =>
              Audio.Volume_Control.Space,
            when Channel_Output_Control_IO_Address =>
              Audio.Output_Control.Space,
            when Power_Control_Status_IO_Address =>
              Read_Power_Control_Status (Audio),
            when Wave_Table_IO_Address => -- TODO consistent range names
              Audio.Wave.Read_Table (Address),
            when others =>
              Blank_Value);
      Put ("Read @");
      Put (Integer (Address), Base => 16, Width => 0);
      Put (' ');
      Put (Integer (Value), Base => 16, Width => 0);
      New_Line;
   end Read;

   procedure Write
     (Audio   : in out Audio_Type;
      Address : Audio_IO_Address;
      Value   : Byte)
   is
   begin
      Put ("Write @");
      Put (Integer (Address), Base => 16, Width => 0);
      Put (' ');
      Put (Integer (Value), Base => 16, Width => 0);
      New_Line;
      if not Audio.Powered and
        (Address in Output_Volume_Control_IO_Address or
        Address in Channel_Output_Control_IO_Address)
      then return; end if;
      case Address is
         when NR1_IO_Address =>
            --  null;
            Audio.Square_1.Write (To_Channel_Register (Address), Value);
         when NR2_IO_Address =>
            --  null; --
            Audio.Square_2.Write (To_Channel_Register (Address), Value);
         when NR3_IO_Address =>
            Audio.Wave.Write (To_Channel_Register (Address), Value);
         when NR4_IO_Address =>
            --  null; --
            Audio.Noise.Write (To_Channel_Register (Address), Value);
         when Output_Volume_Control_IO_Address =>
            Audio.Volume_Control.Space := Value;
         when Channel_Output_Control_IO_Address =>
            Audio.Output_Control.Space := Value;
         when Power_Control_Status_IO_Address =>
            Write_Power_Control_Status (Audio, Value);
         when Wave_Table_IO_Address => -- TODO consistent range names
            Audio.Wave.Write_Table (Address, Value);
         when others => null;
      end case;
   end Write;

   procedure Tick_Frame_Sequencer (Audio : in out Audio_Type) is
   begin
      Audio.Rem_Frame_Seq_Ticks := Audio.Rem_Frame_Seq_Ticks - Audio.Frame_Seq_Step;
      if Audio.Rem_Frame_Seq_Ticks = 0 then
         Audio.Frame_Seq_Step_Idx := Audio.Frame_Seq_Step_Idx + 1;
         case Frame_Sequencer_Steps (Audio.Frame_Seq_Step_Idx) is
            when Length_Counter =>
               Tick_Length (Audio.Square_1);
               Tick_Length (Audio.Square_2);
               Tick_Length (Audio.Wave);
               Tick_Length (Audio.Noise);
            when Length_Counter_Frequency_Sweep =>
               Tick_Length (Audio.Square_1);
               Tick_Length (Audio.Square_2);
               Tick_Length (Audio.Wave);
               Tick_Length (Audio.Noise);
               Tick_Frequency_Sweep (Audio.Square_1);
            when Volume_Envelope => null;
               Tick_Volume_Envelope (Audio.Square_1);
               Tick_Volume_Envelope (Audio.Square_2);
               Tick_Volume_Envelope (Audio.Noise);
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
      Output_Control : Channel_Output_Control renames Audio.Output_Control;

      Target_Cycles : constant Natural := Audio.Elapsed_Cycles + Cycles / 4;

      Enabled_Disabled_Values : array (Boolean) of Sample;

      L_Out, R_Out : Sample;
      Samples : Channel_Samples;
   begin
      Enabled_Disabled_Values (False) := 0;
      while Audio.Elapsed_Cycles < Target_Cycles loop
         Next_Sample (Audio.Square_1, Samples (NR1));
         Next_Sample (Audio.Square_2, Samples (NR2));
         Next_Sample (Audio.Wave, Samples (NR3));
         Next_Sample (Audio.Noise, Samples (NR4));

         Tick_Frame_Sequencer (Audio);

         L_Out := 0;
         R_Out := 0;
         for Ch in Channel_Id loop
            Enabled_Disabled_Values (True) := Samples (Ch);
            L_Out := L_Out + Enabled_Disabled_Values (Output_Control.Left (Ch));
            R_Out := R_Out + Enabled_Disabled_Values (Output_Control.Right (Ch));
         end loop;

         Audio_Buffer (Audio.Elapsed_Cycles) := (L_Out * 16, R_Out * 16);
         Audio.Elapsed_Cycles := Audio.Elapsed_Cycles + 1;
      end loop;
   exception
      when E : others => Put_Line (Exception_Information (E));
   end Report_Cycles;

   function Read_Power_Control_Status (Audio : Audio_Type) return Byte is
      Power_Control : Power_Control_Status renames Audio.Power_Control;
      B : Boolean;
   begin
      B := Audio.Square_1.Enabled;
      Power_Control.Length_Status (NR1) := B;
      B := Audio.Square_2.Enabled;
      Power_Control.Length_Status (NR2) := B;
      Power_Control.Length_Status (NR3) := Enabled (Audio.Wave);
      Power_Control.Length_Status (NR4) := Enabled (Audio.Noise);

      return Power_Control.Space;
   end Read_Power_Control_Status;

   procedure Write_Power_Control_Status
     (Audio : in out Audio_Type;
      Value : Byte)
   is
      New_Power_State : Boolean;
   begin
      Audio.Power_Control.Space := Value or Power_Control_Status_Write_Mask;
      New_Power_State := Audio.Power_Control.Power;
      if Audio.Powered and not New_Power_State then
         Put_Line ("===================== APU Power OFF =====================");
         --  TODO: Powering down should clear and prevent accessing most registers
         Audio.Square_1.Turn_Off;
         Audio.Square_2.Turn_Off;
         Audio.Wave.Turn_Off;
         Audio.Noise.Turn_Off;
         Audio.Volume_Control.Space := 0;
         Audio.Output_Control.Space := 0;

         Audio.Frame_Seq_Step := 0;
      elsif not Audio.Powered and New_Power_State then
         Put_Line ("===================== APU Power ON =====================");
         Audio.Square_1.Turn_On;
         Audio.Square_2.Turn_On;
         Audio.Wave.Turn_On;
         Audio.Noise.Turn_On;
         --  Unusre about this
         --  Audio.Rem_Frame_Seq_Ticks := Samples_Frame_Sequencer_Tick;
         --   Audio.Frame_Seq_Step_Idx := 0;
         --  When powered on, the frame sequencer is reset so that the next step
         --  will be 0
         Audio.Frame_Seq_Step_Idx := 7;
         Audio.Rem_Frame_Seq_Ticks := Samples_Frame_Sequencer_Tick;
         Audio.Frame_Seq_Step := 1;
         Tick_Frame_Sequencer (Audio);
      end if;
      Audio.Powered := New_Power_State;
   end Write_Power_Control_Status;

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

   function Current_Frame_Sequencer_Step
     (Audio : Audio_Type)
      return Frame_Sequencer_Step
   is
   begin
      return Frame_Sequencer_Steps (Audio.Frame_Seq_Step_Idx);
   end Current_Frame_Sequencer_Step;

end Gade.Audio;
