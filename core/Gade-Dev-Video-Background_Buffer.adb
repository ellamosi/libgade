with Gade.Dev.VRAM; use Gade.Dev.VRAM;
with Gade.Dev.Video.Tile_Map; use Gade.Dev.Video.Tile_Map;

package body Gade.Dev.Video.Background_Buffer is

   procedure Reset
     (Buffer : out Background_Buffer_Type) is
   begin
      Buffer (Low_Data, Low_Map).Rasterized := False;
      Buffer (High_Data, Low_Map).Rasterized := False;
      Buffer (Low_Data, High_Map).Rasterized := False;
      Buffer (High_Data, High_Map).Rasterized := False;
   end Reset;

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

--     procedure Rasterize_Tile
--       (Buffer      : in out Concrete_Buffer_Type;
--        Tile_Buffer : Gade.Dev.Video.Tile_Buffer.Tile_Buffer_Type;
--        Map_Row     : Vertical_Tile_Range;
--        Map_Col     : Horizontal_Tile_Range)
--     is
--        Bitmap_Row : Background_Vertical_Range;
--        Bitmap_Col : Background_Horizontal_Range;
--        Tile_Index : Tile_Index_Type;
--     begin
--        Tile_Index := Buffer.Indexes(Map_Row, Map_Col);
--        for Tile_Row in 0..7 loop
--           Bitmap_Row := Natural(Map_Row * 8) + Tile_Row;
--           for Tile_Col in 0..7 loop
--              Bitmap_Col := Natural(Map_Col * 8) + Tile_Col;
--              Buffer.Bitmap(Bitmap_Row, Bitmap_Col) :=
--                Read_Raster_Tile(Tile_Buffer, Tile_Index, Tile_Row, Tile_Col);
--           end loop;
--        end loop;
--     end Rasterize_Tile;
--
--     procedure Rasterize
--       (Buffer      : in out Concrete_Buffer_Type;
--        Tile_Buffer : Gade.Dev.Video.Tile_Buffer.Tile_Buffer_Type;
--        VRAM        : Gade.Dev.VRAM.VRAM_Type;
--        Map         : Tile_Map_Access_Type;
--        Data        : Tile_Data_Access_Type)
--     is
--        Actual_Index : Tile_Index_Type;
--        Tile_Index_Changed : Boolean;
--     begin
--        for Map_Row in Vertical_Tile_Range loop
--           for Map_Col in Horizontal_Tile_Range loop
--              Actual_Index :=
--                VRAM.Consolidated_Maps(Map)(Data).Positional
--                  (Vertical_Tile_Map_Range(Map_Row),
--                   Horizontal_Tile_Map_Range(Map_Col));
--              Tile_Index_Changed := Actual_Index /= Buffer.Indexes(Map_Row, Map_Col);
--              Buffer.Indexes(Map_Row, Map_Col) := Actual_Index;
--              if Tile_Index_Changed or Is_Dirty(Tile_Buffer, Actual_Index) then
--                 Rasterize_Tile(Buffer, Tile_Buffer, Map_Row, Map_Col);
--              end if;
--           end loop;
--        end loop;
--     end Rasterize;
--
--     procedure Rasterize
--       (Buffer      : in out Background_Buffer_Type;
--        Tile_Buffer : Gade.Dev.Video.Tile_Buffer.Tile_Buffer_Type;
--        VRAM        : Gade.Dev.VRAM.VRAM_Type;
--        Tile_Map    : Tile_Map_Access_Type;
--        Tile_Data   : Tile_Data_Access_Type) is
--     begin
--        if not Buffer(Tile_Data, Tile_Map).Rasterized then
--           Rasterize(Buffer(Tile_Data, Tile_Map), Tile_Buffer, VRAM, Tile_Map, Tile_Data);
--           Buffer(Tile_Data, Tile_Map).Rasterized := True;
--        end if;
--     end Rasterize;
--
--     function Read_Raster_Background
--       (Buffer    : Background_Buffer_Type;
--        Row, Col  : Natural;
--        Tile_Map  : Tile_Map_Access_Type;
--        Tile_Data : Tile_Data_Access_Type) return Color_Value is
--     begin
--        return Buffer(Tile_Data, Tile_Map).Bitmap(Row, Col);
--     end Read_Raster_Background;
--
--     procedure Flush (Buffer : in out Background_Buffer_Type) is
--     begin
--        Reset(Buffer);
--     end Flush;

end Gade.Dev.Video.Background_Buffer;
