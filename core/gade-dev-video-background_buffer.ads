limited with Gade.Dev.VRAM;

package Gade.Dev.Video.Background_Buffer is

   function Read
     (VRAM      : Gade.Dev.VRAM.VRAM_Type;
      Row, Col  : Integer;
      Tile_Map  : Tile_Map_Access_Type;
      Tile_Data : Tile_Data_Access_Type) return Color_Value;

end Gade.Dev.Video.Background_Buffer;
