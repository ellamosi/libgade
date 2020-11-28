with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Gade.Audio.Mixer is

   procedure Create (Mixer    : out Audio_Mixer;
                     Square_1 : not null Sweeping_Square_Channel_Access;
                     Square_2 : not null Square_Channel_Access;
                     Wave     : not null Wave_Channel_Access;
                     Noise    : not null Noise_Channel_Access) is
   begin
      Mixer.Square_1 := Square_1;
      Mixer.Square_2 := Square_2;
      Mixer.Wave := Wave;
      Mixer.Noise := Noise;
   end Create;

   procedure Reset (Mixer : in out Audio_Mixer) is
   begin
      Mixer.Turn_Off;
      Mixer.Turn_On;
   end Reset;

   procedure Turn_Off (Mixer : in out Audio_Mixer) is
   begin
      Mixer.Volume_Control.Space := 0;
      Mixer.Channel_Control.Space := 0;
      Mixer.Volume_Multiplier := (Sample_Multiplier, Sample_Multiplier);
      Mixer.Power_Mask := Off_Power_Mask;
   end Turn_Off;

   procedure Turn_On (Mixer : in out Audio_Mixer) is
   begin
      Mixer.Power_Mask := On_Power_Mask;
   end Turn_On;

   function "+" (S1, S2 : Stereo_Sample) return Stereo_Sample is
   begin
      return (S1.Left + S2.Left, S1.Left + S2.Right);
   end "+";

   function "*" (S1, S2 : Stereo_Sample) return Stereo_Sample is
   begin
      return (S1.Left * S2.Left, S1.Right * S2.Right);
   end "*";

   function Next_Sample (Mixer : Audio_Mixer) return Stereo_Sample is
      Output_Control : Channel_Output_Control renames Mixer.Channel_Control;

      Output  : Stereo_Sample;
      Samples : Channel_Samples;
   begin
      Mixer.Square_1.Next_Sample (Samples (NR1));
      Mixer.Square_2.Next_Sample (Samples (NR2));
      Mixer.Wave.Next_Sample (Samples (NR3));
      Mixer.Noise.Next_Sample (Samples (NR4));

      Output := (0, 0);
      for Ch in Channel_Id loop
         Output := Output +
           (Samples (Ch) * (if Output_Control.Left (Ch) then 1 else 0),
            Samples (Ch) * (if Output_Control.Right (Ch) then 1 else 0));
      end loop;

      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Mixer
      --
      --  These multiply the signal by (volume+1). The volume step relative
      --  to the channel DAC is such that a single channel enabled via NR51
      --  playing at volume of 2 with a master volume of 7 is about as loud
      --  as that channel playing at volume 15 with a master volume of 0.
      return Output * Mixer.Volume_Multiplier;
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         return (0, 0);
   end Next_Sample;

   procedure Write_NR50 (Mixer : in out Audio_Mixer; Value : Byte) is
   begin
      Mixer.Volume_Control.Space := Value and Mixer.Power_Mask;
      Mixer.Volume_Multiplier :=
        ((Sample (Mixer.Volume_Control.Left_Volume) + 1) * Sample_Multiplier,
         (Sample (Mixer.Volume_Control.Right_Volume) + 1) * Sample_Multiplier);
   end Write_NR50;

   procedure Write_NR51 (Mixer : in out Audio_Mixer; Value : Byte) is
   begin
      Mixer.Channel_Control.Space := Value and Mixer.Power_Mask;
   end Write_NR51;

   function Read_NR50 (Mixer : Audio_Mixer) return Byte is
   begin
      return Mixer.Volume_Control.Space;
   end Read_NR50;

   function Read_NR51 (Mixer : Audio_Mixer) return Byte is
   begin
      return Mixer.Channel_Control.Space;
   end Read_NR51;

end Gade.Audio.Mixer;
