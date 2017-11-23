with Gade.Dev.Video.Tile_Buffer; use Gade.Dev.Video.Tile_Buffer;
limited with Gade.Dev.VRAM;
with Gade.Video_Buffer; use Gade.Video_Buffer;
with Gade.Dev.OAM; use Gade.Dev.OAM;

package Gade.Dev.Video.Sprites is

   type Sprite_Size_Type is (Single, Double);

   Sprite_Transparent_Color : constant := 0;

   type Sprite_Result_Type is record
      Value    : Color_Value;
      Priority : Object_Priority_Type;
      Palette  : Object_Palette_Type;
   end record;

   type Sprite_Line_Cache is array (Display_Horizontal_Range) of Sprite_Result_Type;

   subtype Edge_Count_Type is Natural range 0 .. 10;
   type Edge_Counts_Type is array (-7 .. 166) of Edge_Count_Type;

   procedure Populate_Line_Cache
     (VRAM  : Gade.Dev.VRAM.VRAM_Type;
      OAM   : Gade.Dev.OAM.OAM_Type;
      Cache : out Sprite_Line_Cache;
      Timings : out Edge_Counts_Type; -- Should probably break this up to a different metho
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

   type Sprite_Index_Array is array (Positive range <>) of Sprite_Index_Type;

   type Sprite_Priority_Buffer is record
      Indexes   : Sprite_Index_Array (1 .. Max_Line_Sprites);
      N_Sprites : Natural;
   end record;

   procedure Insert_By_Processing_Priority
     (Buffer   : in out Sprite_Priority_Buffer;
      Index    : Sprite_Index_Type);

   procedure Insert_By_Draw_Priority
     (Buffer  : in out Sprite_Priority_Buffer;
      Index   : Sprite_Index_Type;
      Sprites : Sprite_Array_Type);

   procedure Prioritize_Sprites
     (Buffer  : out Sprite_Priority_Buffer;
      Sprites : Sprite_Array_Type;
      Row     : Display_Vertical_Range;
      Size    : Sprite_Size_Type);

   procedure Populate_Sprite_Line
     (VRAM   : Gade.Dev.VRAM.VRAM_Type;
      Sprite : Sprite_Type;
      Cache  : in out Sprite_Line_Cache;
      Row    : Display_Vertical_Range;
      Size   : Sprite_Size_Type);

   procedure Populate_Timing_Cache
     (Buffer  : Sprite_Priority_Buffer;
      Sprites : Sprite_Array_Type;
      Timings : out Edge_Counts_Type);

end Gade.Dev.Video.Sprites;
