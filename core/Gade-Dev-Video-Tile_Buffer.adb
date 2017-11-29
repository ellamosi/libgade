with Gade.Dev.VRAM; use Gade.Dev.VRAM;

package body Gade.Dev.Video.Tile_Buffer is

   procedure Reset (Buffer : out Tile_Buffer_Type) is
   begin
      Buffer.Tiles :=
        (others =>
           (State => Dirty,
            Tile  => (others => (others => 0))));
      Buffer.Has_Dirty_Tiles := True;
   end Reset;

   procedure Rasterize_Line
     (Raster : in out Raster_Tile_Line;
      Actual : Tile_Line_Type)
   is
      Low_Value, High_Value, Masked_High, Masked_Low : Byte;
   begin
      Low_Value := Actual.Low;
      High_Value := Actual.High;
      for Col in reverse 0 .. 7 loop
         Masked_Low  := Low_Value and 1;
         Masked_High := High_Value and 1;
         Raster (Col) := Half_Color_Lookup_Table (Masked_Low, Masked_High);
         Low_Value := Low_Value / 2;
         High_Value := High_Value / 2;
      end loop;
   end Rasterize_Line;

   procedure Rasterize_Tile
     (Raster : in out Raster_Tile_Type;
      Actual : Tile_Type)
   is
   begin
      for Row in 0 .. 7 loop
         Rasterize_Line (Raster (Row), Actual (Row));
      end loop;
   end Rasterize_Tile;

   procedure Rasterize_Tiles
     (Buffer : in out Tile_Buffer_Type;
      VRAM   : Gade.Dev.VRAM.VRAM_Type)
   is
   begin
      if Buffer.Has_Dirty_Tiles then
         for Index in Buffer.Tiles'Range loop
            if Buffer.Tiles (Index).State = Dirty then
               Rasterize_Tile
                 (Buffer.Tiles (Index).Tile,
                  VRAM.Map.Tile_Data.All_Tile_Data (Integer (Index))); -- TODO: Casting should not be necessary
               Buffer.Tiles (Index).State := Read;
            end if;
         end loop;
      end if;
      Buffer.Has_Dirty_Tiles := False;
   end Rasterize_Tiles;

   procedure Set_Dirty_Tile
     (Buffer   : in out Tile_Buffer_Type;
      Address  : Word)
   is
      Index : Tile_Index_Type;
   begin
      Index := Tile_Index_Type (Address / Tile_Byte_Size);
      Buffer.Tiles (Index).State := Dirty;
      Buffer.Has_Dirty_Tiles := True;
   end Set_Dirty_Tile;

   procedure Rasterize_Tile
     (Buffer  : in out Tile_Buffer_Type;
      VRAM    : Gade.Dev.VRAM.VRAM_Type;
      Address : Word)
   is
      Index : Tile_Index_Type;
   begin
      Index := Tile_Index_Type (Address / Tile_Byte_Size);
      Rasterize_Tile
        (Buffer.Tiles (Index).Tile,
         VRAM.Map.Tile_Data.All_Tile_Data (Integer (Index)));
   end Rasterize_Tile;

   function Read_Raster_Tile
     (Buffer     : Tile_Buffer_Type;
      Tile_Index : Tile_Index_Type;
      Row, Col   : Natural) return Color_Value
   is
   begin
      return Buffer.Tiles (Tile_Index).Tile (Row)(Col);
   end Read_Raster_Tile;

end Gade.Dev.Video.Tile_Buffer;
