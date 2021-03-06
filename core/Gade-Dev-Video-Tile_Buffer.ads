with Gade.Video_Buffer; use Gade.Video_Buffer;
limited with Gade.Dev.VRAM;

package Gade.Dev.Video.Tile_Buffer is

   Total_Tiles : constant := 256 + 128;

   subtype Tile_Index_Type is Natural range 0 .. Total_Tiles - 1;

   type Tile_Buffer_Type is private;

   type Tile_Buffer_Access_Type is (Low, High, Sprite_8, Sprite_16);

   procedure Reset (Buffer : out Tile_Buffer_Type);

   function Read_Raster_Tile
     (Buffer     : Tile_Buffer_Type;
      Tile_Index : Tile_Index_Type;
      Row, Col   : Natural) return Color_Value;

   --  Rasterizes the tile belonging to the given address
   procedure Rasterize_Tile
     (Buffer  : in out Tile_Buffer_Type;
      VRAM    : Gade.Dev.VRAM.VRAM_Type;
      Address : Word);

private

   Tile_Byte_Size : constant := 16;

   type Raster_Tile_Line is array (0 .. 7) of Color_Value;

   type Raster_Tile_Type is array (0 .. 7) of Raster_Tile_Line;

   type Tile_Buffer_Type is array (Tile_Index_Type) of Raster_Tile_Type;

   Half_Color_Lookup_Table : constant
     array (Byte range 0 .. 1, Byte range 0 .. 1) of Color_Value :=
     ((0, 2), (1, 3));

   procedure Rasterize_Line
     (Raster : in out Raster_Tile_Line;
      Actual : Gade.Dev.VRAM.Tile_Line_Type);

   procedure Rasterize_Tile
     (Raster : in out Raster_Tile_Type;
      Actual : Gade.Dev.VRAM.Tile_Type);

end Gade.Dev.Video.Tile_Buffer;
