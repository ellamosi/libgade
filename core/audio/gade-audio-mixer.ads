private package Gade.Audio.Mixer is

   subtype Output_Volume_Control_IO_Address is Audio_IO_Address
   range 16#FF24# .. 16#FF24#;

   subtype Channel_Output_Control_IO_Address is Audio_IO_Address
   range 16#FF25# .. 16#FF25#;


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


   type Channel_Samples is array (Channel_Id) of Sample;

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

end Gade.Audio.Mixer;
