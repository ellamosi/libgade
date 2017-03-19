with Gade.Video_Buffer;          use Gade.Video_Buffer;
with Gade.Dev.Video.Tile_Buffer; use Gade.Dev.Video.Tile_Buffer;
limited with Gade.Dev.VRAM;

package Gade.Dev.Video.Background_Buffer is

   function Read
     (VRAM      : Gade.Dev.VRAM.VRAM_Type;
      Row, Col  : Integer;
      Tile_Map  : Tile_Map_Access_Type;
      Tile_Data : Tile_Data_Access_Type) return Color_Value;

private

   Vertical_Tiles   : constant := 32;
   Horizontal_Tiles : constant := 32;

   type Vertical_Tile_Range is range 0..Vertical_Tiles-1;
   type Horizontal_Tile_Range is range 0..Horizontal_Tiles-1;

   type Bitmap_Type is array
     (Background_Vertical_Range, Background_Horizontal_Range) of Color_Value;

   type Dirty_Bitmap_Type is array
     (Vertical_Tile_Range, Horizontal_Tile_Range) of Boolean;

   type Tile_Index_Bitmap_Type is array
     (Vertical_Tile_Range, Horizontal_Tile_Range) of Tile_Index_Type;

   type Concrete_Buffer_Type is record
      Bitmap     : Bitmap_Type;
      Indexes    : Tile_Index_Bitmap_Type;
      Rasterized : Boolean;
   end record;

   type Background_Buffer_Type is array
     (Tile_Data_Access_Type, Tile_Map_Access_Type) of Concrete_Buffer_Type;

end Gade.Dev.Video.Background_Buffer;
