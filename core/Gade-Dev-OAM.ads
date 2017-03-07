with Gade.Video_Buffer; use Gade.Video_Buffer;

package Gade.Dev.OAM is

   subtype OAM_IO_Address is Word range 16#FE00#..16#FE9F#;

   type Object_Palette_Type is (OBJ0PAL, OBJ1PAL); -- Should not be declared here

   -- Sprites are either 8x8 or 8x16
   type Sprite_Type is record
      -- TODO: Type properly!
      Y, X, Pattern  : Byte;
      Priority       : Boolean;
      Y_Flip, X_Flip : Boolean;
      Palette        : Object_Palette_Type;
   end record;
   for Sprite_Type use record
      Y        at 0 range 0..7;
      X        at 1 range 0..7;
      Pattern  at 2 range 0..7;
      Palette  at 3 range 4..4;
      X_Flip   at 3 range 5..5;
      Y_Flip   at 3 range 6..6;
      Priority at 3 range 7..7;
   end record;
   for Sprite_Type'Size use 8*4;

   type Sprite_Array_Type is array (0..39) of Sprite_Type;
   type VRAM_Access_Type is (Named, Address);

   type OAM_Address_Space is array (OAM_IO_Address) of Byte;

   type OAM_Map_Type (Access_Type : VRAM_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            Sprites : Sprite_Array_Type;
         when Address =>
            Space : OAM_Address_Space;
      end case;
   end record;
   pragma Unchecked_Union (OAM_Map_Type);

   type OAM_Type is new Memory_Mapped_Device with record
      Map : OAM_Map_Type;
   end record;

   type OAM_Access is access all OAM_Type;

   overriding procedure Reset
     (OAM : in out OAM_Type);

   overriding procedure Read
     (OAM     : in out OAM_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte);

   overriding procedure Write
     (OAM     : in out OAM_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : Byte);

end Gade.Dev.OAM;
