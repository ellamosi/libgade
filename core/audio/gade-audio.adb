with Ada.Text_IO; use Ada.Text_IO;
with Gade.Audio.Channels; use Gade.Audio.Channels;
with Gade.Audio.Channels.Pulse; use Gade.Audio.Channels.Pulse;
with Gade.Audio.Channels.Pulse.Noise; use Gade.Audio.Channels.Pulse.Noise;
with Gade.Audio.Channels.Pulse.Square; use Gade.Audio.Channels.Pulse.Square;
with Gade.Audio.Channels.Wave; use Gade.Audio.Channels.Wave;
with Gade.Audio.Mixer; use Gade.Audio.Mixer;

with Gade.Audio.Channels.Pulse.Square.Sweeping;
use Gade.Audio.Channels.Pulse.Square.Sweeping;

package body Gade.Audio is

   function To_Channel_Register
     (Address : Audio_IO_Address)
      return Channel_Register;

   type Opaque_Audio_Type is record
      Square_1 : aliased Sweeping_Square_Channel;
      Square_2 : aliased Square_Channel;
      Wave     : aliased Wave_Channel;
      Noise    : aliased Noise_Channel;

      Power_Control  : Power_Control_Status;

      Mixer : Audio_Mixer;

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

      Create
        (Audio.Mixer,
         Audio.Square_1'Access,
         Audio.Square_2'Access,
         Audio.Wave'Access,
         Audio.Noise'Access);
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

      Reset (Audio.Mixer);

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
      Target_Cycles : constant Natural := Audio.Elapsed_Cycles + Cycles / 4;
   begin
      while Audio.Elapsed_Cycles < Target_Cycles loop
         Audio_Buffer (Audio.Elapsed_Cycles) := Audio.Mixer.Next_Sample;

         Tick_Frame_Sequencer (Audio);

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
         Put_Line ("===================== APU Power OFF =====================");
         Audio.Square_1.Turn_Off;
         Audio.Square_2.Turn_Off;
         Audio.Wave.Turn_Off;
         Audio.Noise.Turn_Off;
         Audio.Mixer.Turn_Off;

         Audio.Frame_Seq_Step := 0;
      elsif not Audio.Powered and New_Power_State then
         Put_Line ("===================== APU Power ON =====================");
         Audio.Square_1.Turn_On;
         Audio.Square_2.Turn_On;
         Audio.Wave.Turn_On;
         Audio.Noise.Turn_On;
         Audio.Mixer.Turn_On;
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
      Audio.Elapsed_Cycles := 0;
   end Flush_Frame;

   function Current_Frame_Sequencer_Step
     (Audio : Audio_Type)
      return Frame_Sequencer_Step
   is
   begin
      return Frame_Sequencer_Steps (Audio.Frame_Seq_Step_Idx);
   end Current_Frame_Sequencer_Step;

   function Is_Powered (Audio : Audio_Type) return Boolean is
   begin
      return Audio.Powered;
   end Is_Powered;

end Gade.Audio;
