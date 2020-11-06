with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Gade.Audio.Mixer is

   procedure Create
     (Mixer    : out Audio_Mixer;
      Square_1 : not null Sweeping_Square_Channel_Access;
      Square_2 : not null Square_Channel_Access;
      Wave     : not null Wave_Channel_Access;
      Noise    : not null Noise_Channel_Access)
   is
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
      Mixer.Output_Control.Space := 0;
      Mixer.Volume := (1, 1); -- TODO: use Sample_Mult
      Mixer.Power_Mask := Off_Power_Mask;
   end Turn_Off;

   procedure Turn_On (Mixer : in out Audio_Mixer) is
   begin
      Mixer.Power_Mask := On_Power_Mask;
   end Turn_On;

   function Next_Sample (Mixer : Audio_Mixer) return Stereo_Sample is
      Enabled_Disabled_Values : array (Boolean) of Sample;

      L_Out, R_Out : Sample;
      Samples : Channel_Samples;
   begin
      Enabled_Disabled_Values (False) := 0; -- TODO: Should not happen per sample

      Mixer.Square_1.Next_Sample (Samples (NR1));
      Mixer.Square_2.Next_Sample (Samples (NR2));
      Mixer.Wave.Next_Sample (Samples (NR3));
      Mixer.Noise.Next_Sample (Samples (NR4));

      L_Out := 0;
      R_Out := 0;
      for Ch in Channel_Id loop
         --  TODO: Simplify this
         Enabled_Disabled_Values (True) := Samples (Ch);
         L_Out := L_Out + Enabled_Disabled_Values (Mixer.Output_Control.Left (Ch));
         R_Out := R_Out + Enabled_Disabled_Values (Mixer.Output_Control.Right (Ch));
      end loop;

      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Mixer
      --
      --  These multiply the signal by (volume+1). The volume step relative
      --  to the channel DAC is such that a single channel enabled via NR51
      --  playing at volume of 2 with a master volume of 7 is about as loud
      --  as that channel playing at volume 15 with a master volume of 0.
      return (L_Out * Mixer.Volume.Left, R_Out * Mixer.Volume.Right);
   exception
      when E : others =>
         --  TODO: Clean this up
         --  60 * 576 = 34560 > 32767
         --  15 * 4 = 60
         --  15 * 4 * (7+1) = 480 (Max unmultiplied level)
         --  32767 / 480 = 68.27;
         --  60 * 8 * 68 = 60 * 544 = 32640

         Put_Line ("L_Out" & L_Out'Img &
                     " Mult" & Mixer.Volume.Left'Img &
                     " LV" & Mixer.Volume_Control.Left_Volume'Img);
         for Ch in Channel_Id loop
            Put (Ch'Img & Samples (Ch)'Img);
         end loop;
         New_Line;
         Put_Line (Exception_Information (E));
         return (0, 0);
   end Next_Sample;

   procedure Write_NR50 (Mixer : in out Audio_Mixer; Value : Byte) is
      --  TODO: Change channel sample type and move formula to spec
      --  32767 / (15 * 4 * 8) = 32767 / 480 = 68.26
      Sample_Mult : constant Sample := Sample'Last / (15 * Channel_Count * (7 + 1));
   begin
      Mixer.Volume_Control.Space := Value and Mixer.Power_Mask;
      Mixer.Volume :=
        ((Sample (Mixer.Volume_Control.Left_Volume) + 1) * Sample_Mult,
         (Sample (Mixer.Volume_Control.Right_Volume) + 1) * Sample_Mult);
   end Write_NR50;

   procedure Write_NR51 (Mixer : in out Audio_Mixer; Value : Byte) is
   begin
      Mixer.Output_Control.Space := Value and Mixer.Power_Mask;
   end Write_NR51;

   function Read_NR50 (Mixer : Audio_Mixer) return Byte is
   begin
      return Mixer.Volume_Control.Space;
   end Read_NR50;

   function Read_NR51 (Mixer : Audio_Mixer) return Byte is
   begin
      return Mixer.Output_Control.Space;
   end Read_NR51;

end Gade.Audio.Mixer;
