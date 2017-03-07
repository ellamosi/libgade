package body Gade.Dev.VRAM is

   procedure Reset (VRAM : in out VRAM_Type) is
   begin
      VRAM.Map.Space := (others => 0);
   end Reset;

   procedure Read
     (VRAM    : in out VRAM_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte) is
   begin
      Value := VRAM.Map.Space(Address);
   end Read;

   procedure Write
     (VRAM    : in out VRAM_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : Byte) is
   begin
      VRAM.Map.Space(Address) := Value;
   end Write;

   function Tile_Color
      (Line   : Tile_Line_Type;
       Column : Integer) return Color_Value is
      Bit_Shift   : Byte;
      Masked_High : Byte;
      Masked_Low  : Byte;
   begin
      Bit_Shift   := 2 ** (7-Column);
      Masked_Low  := (Line.Low / Bit_Shift) and 1;
      Masked_High := (Line.High / Bit_Shift) and 1;
      return Half_Color_Lookup_Table(Masked_Low, Masked_High);
   end Tile_Color;

end Gade.Dev.VRAM;
