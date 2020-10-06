with Ada.Text_IO; use Ada.Text_IO;
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

      Wave_Table : aliased Wave_Table_IO;

      Elapsed_Cycles   : Natural;
      Frame_Seq_Step_Idx : Frame_Sequencer_Step_Index;
      Rem_Frame_Seq_Ticks : Natural;
   end record;

   procedure Create (Audio : aliased out Audio_Type) is
   begin
      Audio := new Opaque_Audio_Type;
      Audio.Wave.Set_Table (Audio.Wave_Table'Access);
   end Create;

   procedure Reset (Audio : in out Audio_Type) is
   begin
      Put_Line ("Audio Reset");

      Audio.Elapsed_Cycles := 0;
      Audio.Frame_Seq_Step_Idx := 0;
      Audio.Rem_Frame_Seq_Ticks := Samples_Frame_Sequencer_Tick;

      Reset (Audio.Square_1);
      Reset (Audio.Square_2);
      Reset (Audio.Wave);
      Reset (Audio.Noise);

      --  TODO: Initialize wave table pattern somehow
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
            when NR1_Address =>
              Audio.Square_1.Read (To_Channel_Register (Address)),
            when NR2_Address =>
              Audio.Square_2.Read (To_Channel_Register (Address)),
            when NR3_Address =>
              Audio.Wave.Read (To_Channel_Register (Address)),
            when NR4_Address =>
              Audio.Noise.Read (To_Channel_Register (Address)),
            when Control_Address =>
              Blank_Value, -- TODO
            when Wave_Table_IO_Range => -- TODO consistent range names
              Audio.Wave_Table.Space (Address),
            when others =>
              Blank_Value);
   end Read;

   procedure Write
     (Audio   : in out Audio_Type;
      Address : Audio_IO_Address;
      Value   : Byte)
   is
   begin
      case Address is
         when NR1_Address =>
            Audio.Square_1.Write (To_Channel_Register (Address), Value);
         when NR2_Address =>
            Audio.Square_2.Write (To_Channel_Register (Address), Value);
         when NR3_Address =>
            Audio.Wave.Write (To_Channel_Register (Address), Value);
         when NR4_Address =>
            Audio.Noise.Write (To_Channel_Register (Address), Value);
         when Control_Address =>
            null;
         when Wave_Table_IO_Range =>
            Audio.Wave_Table.Space (Address) := Value; -- TODO consistent range names
         when others => null;
      end case;
   end Write;

   procedure Tick_Frame_Sequencer (Audio : in out Audio_Type) is
   begin
      Audio.Rem_Frame_Seq_Ticks := Audio.Rem_Frame_Seq_Ticks - 1;
      if Audio.Rem_Frame_Seq_Ticks = 0 then
         Audio.Frame_Seq_Step_Idx := Audio.Frame_Seq_Step_Idx + 1;
         case Frame_Sequencer_Steps (Audio.Frame_Seq_Step_Idx) is
            when Length_Counter =>
               Tick_Length (Audio.Square_1);
               Tick_Length (Audio.Square_2);
               Tick_Length (Audio.Noise);
            when Length_Counter_Frequency_Sweep =>
               Tick_Length (Audio.Square_1);
               Tick_Length (Audio.Square_2);
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

      S1, S2, S3, S4, S_Out : Sample;
   begin
      while Audio.Elapsed_Cycles < Target_Cycles loop
         S1 := 0;
         S2 := 0;
         S3 := 0;
         S4 := 0;
         Next_Sample (Audio.Square_1, S1);
         Next_Sample (Audio.Square_2, S2);
         Next_Sample (Audio.Wave, S3);
         Next_Sample (Audio.Noise, S4);

         Tick_Frame_Sequencer (Audio);

         S_Out := (S1 + S2 + S3 + S4) * 8 * 32; -- Temporary master volume and dyn range adjustment
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
