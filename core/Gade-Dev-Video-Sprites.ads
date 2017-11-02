with Gade.Dev.Video.Tile_Buffer; use Gade.Dev.Video.Tile_Buffer;
limited with Gade.Dev.VRAM;
with Gade.Video_Buffer; use Gade.Video_Buffer;
with Gade.Dev.OAM; use Gade.Dev.OAM;

package Gade.Dev.Video.Sprites is

   type Sprite_Size_Type is (Single, Double);

   Sprite_Transparent_Color : constant := 0;

   type Sprite_Result_Type is record
      Value    : Color_Value;
      Priority : Boolean;
      Palette  : Object_Palette_Type;
   end record;

   type Sprite_Line_Cache is array (Display_Horizontal_Range) of Sprite_Result_Type;

   procedure Populate_Line_Cache
     (VRAM  : Gade.Dev.VRAM.VRAM_Type;
      OAM   : Gade.Dev.OAM.OAM_Type;
      Cache : out Sprite_Line_Cache;
      Row   : Display_Vertical_Range;
      Size  : Sprite_Size_Type);

private

   Single_Sprite_Height : constant := 8;
   Double_Sprite_Height : constant := 16;

   Max_Line_Sprites : constant := 10;

   Sprite_Height : constant array (Sprite_Size_Type'Range) of Positive :=
     (Single_Sprite_Height, Double_Sprite_Height);

   Sprite_Width : constant := 8;

   subtype Sprite_Vertical_Range is Natural range 0 .. Double_Sprite_Height - 1;
   subtype Sprite_Horizontal_Range is Natural range 0 .. Sprite_Width - 1;

   X_Flip_Lookup : constant array (Boolean'Range, Sprite_Horizontal_Range)
     of Natural :=
       (False => (0, 1, 2, 3, 4, 5, 6, 7),
        True  => (7, 6, 5, 4, 3, 2, 1, 0));

   Y_Flip_Lookup : constant array
     (Sprite_Size_Type'Range, Boolean'Range, Sprite_Vertical_Range)
     of Natural :=
       (Single =>
          (False => (0, 1, 2, 3, 4, 5, 6, 7, others => 0),
           True  => (7, 6, 5, 4, 3, 2, 1, 0, others => 0)),
        Double =>
          (False => (0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7),
           True  => (7, 6, 5, 4, 3, 2, 1, 0, 7, 6, 5, 4, 3, 2, 1, 0)));

   Sprite_Index_Add_Lookup : constant array
     (Sprite_Size_Type'Range, Boolean'Range, Sprite_Vertical_Range)
     of Tile_Index_Type :=
       (Single => (others => (others => 0)),
        Double =>
          (False => (0 .. 7 => 0, 8 .. 15 => 1),
           True  => (0 .. 7 => 1, 8 .. 15 => 0)));

end Gade.Dev.Video.Sprites;
