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

   function Read
     (VRAM      : Gade.Dev.VRAM.VRAM_Type;
      OAM       : Gade.Dev.OAM.OAM_Type;
      Row, Col  : Integer;
      Size      : Sprite_Size_Type) return Sprite_Result_Type;

private

   Single_Sprite_Height : constant := 8;
   Double_Sprite_Height : constant := 16;

   Sprite_Height : constant array (Sprite_Size_Type'Range) of Positive :=
     (Single_Sprite_Height, Double_Sprite_Height);

   Sprite_Width : constant := 8;

   subtype Sprite_Vertical_Range is Natural range 0 .. Double_Sprite_Height - 1;
   subtype Sprite_Horizontal_Range is Natural range 0 .. Sprite_Width - 1;

   type X_Flip_Lookup_Array is array (Sprite_Horizontal_Range) of Natural;

   X_Flip_Lookup : constant array (Boolean'Range) of X_Flip_Lookup_Array :=
     (False => (0, 1, 2, 3, 4, 5, 6, 7),
      True  => (7, 6, 5, 4, 3, 2, 1, 0));

   type Y_Flip_Lookup_Array is array (0 .. 15) of Natural;

   type Y_Flip_Lookup_Size_Array is array (Boolean'Range) of
     Y_Flip_Lookup_Array;

   Y_Flip_Lookup : constant array (Sprite_Size_Type'Range) of
     Y_Flip_Lookup_Size_Array :=
       (Single =>
          (False => (0, 1, 2, 3, 4, 5, 6, 7, others => 0),
           True  => (7, 6, 5, 4, 3, 2, 1, 0, others => 0)),
        Double =>
          (False => (0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7),
           True  => (7, 6, 5, 4, 3, 2, 1, 0, 7, 6, 5, 4, 3, 2, 1, 0)));

   type Sprite_Add_Array_Type is array (Sprite_Vertical_Range) of
     Tile_Index_Type;

   Sprite_Index_Add_Lookup : constant array (Sprite_Size_Type'Range) of
     Sprite_Add_Array_Type :=
       (Single => (others => 0),
        Double => (0 .. 7 => 0, 8 .. 15 => 1));

end Gade.Dev.Video.Sprites;
