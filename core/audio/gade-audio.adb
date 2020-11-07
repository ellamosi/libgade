with Ada.Text_IO; use Ada.Text_IO;

with Gade.Audio.Channels,
     Gade.Audio.Channels.Pulse.Noise,
     Gade.Audio.Channels.Pulse.Square,
     Gade.Audio.Channels.Pulse.Square.Sweeping,
     Gade.Audio.Channels.Wave,
     Gade.Audio.Frame_Sequencer,
     Gade.Audio.Mixer;

use Gade.Audio.Channels,
    Gade.Audio.Channels.Pulse.Noise,
    Gade.Audio.Channels.Pulse.Square,
    Gade.Audio.Channels.Pulse.Square.Sweeping,
    Gade.Audio.Channels.Wave,
    Gade.Audio.Frame_Sequencer,
    Gade.Audio.Mixer;

package body Gade.Audio is

   function To_Channel_Register
     (Address : Audio_IO_Address)
      return Channel_Register;

   type Opaque_Audio_Type is record
      Square_1 : aliased Sweeping_Square_Channel;
      Square_2 : aliased Square_Channel;
      Wave     : aliased Wave_Channel;
      Noise    : aliased Noise_Channel;

      Mixer     : Audio_Mixer;
      Frame_Seq : Frame_Sequencer.Audio_Frame_Sequencer;

      Powered       : Boolean;
      Power_Control : Power_Control_Status;

      Elapsed_Cycles : Natural;
   end record;

   procedure Create (Audio : aliased out Audio_Type) is
   begin
      Audio := new Opaque_Audio_Type;

      Create (Audio.Square_1, Audio);
      Create (Audio.Square_2, Audio);
      Create (Audio.Wave, Audio);
      Create (Audio.Noise, Audio);

      Create
        (Audio.Mixer,
         Audio.Square_1'Access,
         Audio.Square_2'Access,
         Audio.Wave'Access,
         Audio.Noise'Access);
      Create
        (Audio.Frame_Seq,
         Audio.Square_1'Access,
         Audio.Square_2'Access,
         Audio.Wave'Access,
         Audio.Noise'Access);
   end Create;

   procedure Reset (Audio : in out Audio_Type) is
   begin
      Put_Line ("Audio Reset");

      Reset (Audio.Square_1);
      Reset (Audio.Square_2);
      Reset (Audio.Wave);
      Reset (Audio.Noise);

      Reset (Audio.Mixer);
      Reset (Audio.Frame_Seq);

      Audio.Powered := True;
      Audio.Power_Control.Space := Power_Control_Status_Write_Mask;

      Audio.Elapsed_Cycles := 0;
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
            when NR1x_IO_Address =>
              Audio.Square_1.Read (To_Channel_Register (Address)),
            when NR2x_IO_Address =>
              Audio.Square_2.Read (To_Channel_Register (Address)),
            when NR3x_IO_Address =>
              Audio.Wave.Read (To_Channel_Register (Address)),
            when NR4x_IO_Address =>
              Audio.Noise.Read (To_Channel_Register (Address)),
            when NR50_IO_Address =>
              Audio.Mixer.Read_NR50,
            when NR51_IO_Address =>
              Audio.Mixer.Read_NR51,
            when NR52_IO_Address =>
              Read_Power_Control_Status (Audio),
            when Wave_Table_IO_Address =>
              Audio.Wave.Read_Table (Address),
            when others =>
              Blank_Value);
--        if Address not in NR2_IO_Address and Address not in NR3_IO_Address and
--          Address not in NR4_IO_Address and Address not in Wave_Table_IO_Address
--        then
--           Put ("Read @");
--           Put (Integer (Address), Base => 16, Width => 0);
--           Put (' ');
--           Put (Integer (Value), Base => 16, Width => 0);
--           New_Line;
--        end if;
   end Read;

   procedure Write
     (Audio   : in out Audio_Type;
      Address : Audio_IO_Address;
      Value   : Byte)
   is
   begin
--        if Address not in NR2x_IO_Address and Address not in NR3x_IO_Address and
--          Address not in NR4x_IO_Address and Address not in Wave_Table_IO_Address
--        then
--           Put ("Write @");
--           Put (Integer (Address), Base => 16, Width => 0);
--           Put (' ');
--           Put (Integer (Value), Base => 16, Width => 0);
--           New_Line;
--        end if;
      case Address is
         when NR1x_IO_Address =>
            Audio.Square_1.Write (To_Channel_Register (Address), Value);
         when NR2x_IO_Address =>
            Audio.Square_2.Write (To_Channel_Register (Address), Value);
         when NR3x_IO_Address =>
            Audio.Wave.Write (To_Channel_Register (Address), Value);
         when NR4x_IO_Address =>
            Audio.Noise.Write (To_Channel_Register (Address), Value);
         when NR50_IO_Address =>
            Audio.Mixer.Write_NR50 (Value);
         when NR51_IO_Address =>
            Audio.Mixer.Write_NR51 (Value);
         when NR52_IO_Address =>
            Write_Power_Control_Status (Audio, Value);
         when Wave_Table_IO_Address =>
            Audio.Wave.Write_Table (Address, Value);
         when others => null;
      end case;
   end Write;

   procedure Report_Cycles
     (Audio        : in out Audio_Type;
      Audio_Buffer : Audio_Buffer_Access;
      Cycles       : Positive)
   is
      Target_Cycles : constant Natural := Audio.Elapsed_Cycles + Cycles / 4;
   begin
      while Audio.Elapsed_Cycles < Target_Cycles loop
         Audio_Buffer (Audio.Elapsed_Cycles) := Audio.Mixer.Next_Sample;

         Audio.Frame_Seq.Tick;

         Audio.Elapsed_Cycles := Audio.Elapsed_Cycles + 1;
      end loop;
   end Report_Cycles;

   function Read_Power_Control_Status (Audio : Audio_Type) return Byte is
   begin
      Audio.Power_Control.Length_Status (NR1) := Audio.Square_1.Enabled;
      Audio.Power_Control.Length_Status (NR2) := Audio.Square_2.Enabled;
      Audio.Power_Control.Length_Status (NR3) := Audio.Wave.Enabled;
      Audio.Power_Control.Length_Status (NR4) := Audio.Noise.Enabled;

      return Audio.Power_Control.Space;
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
         Audio.Square_1.Turn_Off;
         Audio.Square_2.Turn_Off;
         Audio.Wave.Turn_Off;
         Audio.Noise.Turn_Off;
         Audio.Mixer.Turn_Off;
         Audio.Frame_Seq.Turn_Off;
      elsif not Audio.Powered and New_Power_State then
         Audio.Square_1.Turn_On;
         Audio.Square_2.Turn_On;
         Audio.Wave.Turn_On;
         Audio.Noise.Turn_On;
         Audio.Mixer.Turn_On;
         Audio.Frame_Seq.Turn_On;
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
      Audio.Elapsed_Cycles := 0;
   end Flush_Frame;

   function Frame_Sequencer_State (Audio : Audio_Type)
                                   return Frame_Sequencer.State
   is
   begin
      return Audio.Frame_Seq.Current_State;
   end Frame_Sequencer_State;

   function Is_Powered (Audio : Audio_Type) return Boolean is
   begin
      return Audio.Powered;
   end Is_Powered;

end Gade.Audio;
