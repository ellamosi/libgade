package body Gade.Dev.VRAM is

   overriding
   procedure Reset (VRAM : in out VRAM_Type) is
   begin
      VRAM.Map.Space := (others => 0);
      for Tile in VRAM.Raster.All_Tile_Data'Range loop
         for Row in 0 .. 7 loop
            for Col in 0 .. 7 loop
               VRAM.Raster.All_Tile_Data (Tile)(Row, Col) := 0;
            end loop;
         end loop;
      end loop;
      Reset (VRAM.Tile_Buffer);
   end Reset;

   overriding
   procedure Read
     (VRAM    : in out VRAM_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte) is
      pragma Unreferenced (GB);
   begin
      Value := VRAM.Map.Space (Address);
   end Read;

   overriding
   procedure Write
     (VRAM    : in out VRAM_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : Byte) is
      pragma Unreferenced (GB);
   begin
      VRAM.Map.Space (Address) := Value;
      if Address in 16#8000# .. 16#97FF# then
         Set_Dirty_Tile (VRAM.Tile_Buffer, Address - 16#8000#);
      elsif Address in 16#9800# .. 16#9C00# - 1 then
         Consolidate_Tile_Index (VRAM.Consolidated_Maps (Low_Map), Address, Value);
      elsif Address in 16#9C00# .. 16#A000# - 1 then
         Consolidate_Tile_Index (VRAM.Consolidated_Maps (High_Map), Address, Value);
      end if;
   end Write;

   function Tile_Color
      (Line   : Tile_Line_Type;
       Column : Integer) return Color_Value is
      Bit_Shift   : Byte;
      Masked_High : Byte;
      Masked_Low  : Byte;
   begin
      Bit_Shift   := 2 ** (7 - Column);
      Masked_Low  := (Line.Low / Bit_Shift) and 1;
      Masked_High := (Line.High / Bit_Shift) and 1;
      return Half_Color_Lookup_Table (Masked_Low, Masked_High);
   end Tile_Color;

end Gade.Dev.VRAM;
