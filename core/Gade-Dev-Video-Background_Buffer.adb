with Gade.Dev.VRAM; use Gade.Dev.VRAM;
with Gade.Dev.Video.Tile_Map; use Gade.Dev.Video.Tile_Map;
with Gade.Dev.Video.Tile_Buffer; use Gade.Dev.Video.Tile_Buffer;

package body Gade.Dev.Video.Background_Buffer is

   function Read
     (VRAM      : Gade.Dev.VRAM.VRAM_Type;
      Row, Col  : Integer;
      Tile_Map  : Tile_Map_Access_Type;
      Tile_Data : Tile_Data_Access_Type) return Color_Value
   is
      Tile_Row : Vertical_Tile_Map_Range;
      Tile_Col : Horizontal_Tile_Map_Range;
      Tile_PX_Row, Tile_PX_Col : Natural;
      Tile_Index : Tile_Index_Type;
   begin
      Tile_Row := Vertical_Tile_Map_Range (Row / 8);
      Tile_PX_Row := Row mod 8;
      Tile_Col := Horizontal_Tile_Map_Range (Col / 8);
      Tile_PX_Col := Col mod 8;

      Tile_Index :=
        VRAM.Consolidated_Maps (Tile_Map)(Tile_Data).Positional (Tile_Row, Tile_Col);

      return
        Read_Raster_Tile
          (VRAM.Tile_Buffer,
           Tile_Index,
           Tile_PX_Row,
           Tile_PX_Col);
   end Read;

end Gade.Dev.Video.Background_Buffer;
