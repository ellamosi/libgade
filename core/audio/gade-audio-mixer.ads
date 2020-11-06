with Gade.Audio.Channels.Pulse.Noise,
     Gade.Audio.Channels.Pulse.Square,
     Gade.Audio.Channels.Pulse.Square.Sweeping,
     Gade.Audio.Channels.Wave;

use Gade.Audio.Channels.Pulse.Noise,
    Gade.Audio.Channels.Pulse.Square,
    Gade.Audio.Channels.Pulse.Square.Sweeping,
    Gade.Audio.Channels.Wave;

private package Gade.Audio.Mixer is

   type Audio_Mixer is tagged private;

   procedure Create
     (Mixer    : out Audio_Mixer;
      Square_1 : not null Sweeping_Square_Channel_Access;
      Square_2 : not null Square_Channel_Access;
      Wave     : not null Wave_Channel_Access;
      Noise    : not null Noise_Channel_Access);

   procedure Reset (Mixer : in out Audio_Mixer);

   procedure Turn_Off (Mixer : in out Audio_Mixer);

   procedure Turn_On (Mixer : in out Audio_Mixer);

   function Next_Sample (Mixer : Audio_Mixer) return Stereo_Sample;

   procedure Write_NR50 (Mixer : in out Audio_Mixer; Value : Byte);

   procedure Write_NR51 (Mixer : in out Audio_Mixer; Value : Byte);

   function Read_NR50 (Mixer : Audio_Mixer) return Byte;

   function Read_NR51 (Mixer : Audio_Mixer) return Byte;

private

   On_Power_Mask  : constant Byte := 16#FF#;
   Off_Power_Mask : constant Byte := 16#00#;

   type Channel_Samples is array (Channel_Id) of Sample;


   type Output_Volume is mod 2 ** 3;

   type Output_Volume_Control (S : Audio_Access_Type := Named) is record
      case S is
         when Named =>
            Right_Volume     : Output_Volume;
            Vin_Right_Enable : Boolean;
            Left_Volume      : Output_Volume;
            Vin_Left_Enable  : Boolean;
         when Address =>
            Space            : Byte;
      end case;
   end record with Unchecked_Union;
   for Output_Volume_Control use record
      Right_Volume     at 0 range 0 .. 2;
      Vin_Right_Enable at 0 range 3 .. 3;
      Left_Volume      at 0 range 4 .. 6;
      Vin_Left_Enable  at 0 range 7 .. 7;
      Space            at 0 range 0 .. 7;
   end record;
   for Output_Volume_Control'Size use Byte'Size;


   type Channel_Output_Control (S : Audio_Access_Type := Named) is record
      case S is
         when Named =>
            Right, Left : Channel_Flags;
         when Address =>
            Space       : Byte;
      end case;
   end record with Unchecked_Union;
   for Channel_Output_Control use record
      Right at 0 range 0 .. 3;
      Left  at 0 range 4 .. 7;
      Space at 0 range 0 .. 7;
   end record;
   for Channel_Output_Control'Size use Byte'Size;


   type Audio_Mixer is tagged record
      Square_1 : Sweeping_Square_Channel_Access;
      Square_2 : Square_Channel_Access;
      Wave     : Wave_Channel_Access;
      Noise    : Noise_Channel_Access;

      Output_Control : Channel_Output_Control;
      Volume_Control : Output_Volume_Control;

      Power_Mask : Byte;
      Volume     : Stereo_Sample;
   end record;

end Gade.Audio.Mixer;
