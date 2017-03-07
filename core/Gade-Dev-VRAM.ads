with Gade.Video_Buffer; use Gade.Video_Buffer;

package Gade.Dev.VRAM is

   subtype VRAM_IO_Address is Word range 16#8000#..16#9FFF#;

   type Tile_Line_Type is record
      Low, High : Byte;
   end record;
   for Tile_Line_Type use record
      Low  at 0 range 0..7;
      High at 1 range 0..7;
   end record;
   for Tile_Line_Type'Size use 16;

   function Tile_Color (Line   : Tile_Line_Type;
                        Column : Integer) return Color_Value;

   type Tile_Type is array (0..7) of Tile_Line_Type;
   type Tile_Type_16 is array (0..15) of Tile_Line_Type;

   Half_Color_Lookup_Table : constant
     array (Byte range 0..1, Byte range 0..1) of Color_Value := ((0,2),(1,3));

   type VRAM_Address_Space is array (VRAM_IO_Address) of Byte;

   type Low_Tile_Array_Type is array (Byte'Range) of Tile_Type; -- 8000-8FFF
   for Low_Tile_Array_Type'Size use 8 * 16#1000#; --8*16*256;

   type High_Tile_Array_Type is array (Signed_Byte'Range) of Tile_Type; -- 8800-97FF
   for High_Tile_Array_Type'Size use 8 * 16#1000#; --8*16*256;

   type Obj_8_8_Array_Type is array (Byte'Range) of Tile_Type; -- 8000-8FFF
   for Obj_8_8_Array_Type'Size use 8 * 16#1000#; --8*16*256;

   type Obj_8_16_Array_Type is array (Byte range 0..127) of Tile_Type_16; -- 8000-8FFF
   for Obj_8_16_Array_Type'Size use 8 * 16#1000#; --8*32*128;

   type Tile_Array_Access_Type is (Low, High, Object_8_8, Object_8_16);
   type Tile_Map_Position_Type is (Low, High);

   type Tile_Array_Variant
      (Tile_Array_Access  : Tile_Array_Access_Type := Low) is record
      case Tile_Array_Access is
         when Low =>
            Low_Tile_Data    : Low_Tile_Array_Type;
         when High =>
            High_Tile_Data   : High_Tile_Array_Type;
         when Object_8_8 =>
            Sprite_Data_8_8  : Obj_8_8_Array_Type;
         when Object_8_16 =>
            Sprite_Data_8_16 : Obj_8_16_Array_Type;
      end case;
   end record;
   for Tile_Array_Variant use record
      Low_Tile_Data    at 0       range 0..8 * 16#1000# - 1;
      High_Tile_Data   at 16#800# range 0..8 * 16#1000# - 1;
      Sprite_Data_8_8  at 0       range 0..8 * 16#1000# - 1;
      Sprite_Data_8_16 at 0       range 0..8 * 16#1000# - 1;
   end record;
   pragma Unchecked_Union (Tile_Array_Variant);
   for Tile_Array_Variant'Size use 8 * (16#9800#-16#8000#);

   type Tile_Map_Type is array (0..31, 0..31) of Byte; -- 9800-9BFF or 9C00-9FFF
   for Tile_Map_Type'Size use 8*32*32;

   type VRAM_Access_Type is (Named, Address);

   type VRAM_Map_Type (Access_Type : VRAM_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            Tile_Data     : Tile_Array_Variant;
            Low_Tile_Map  : Tile_Map_Type;
            High_Tile_Map : Tile_Map_Type;
         when Address =>
            Space : VRAM_Address_Space;
      end case;
   end record;
   pragma Unchecked_Union (VRAM_Map_Type);

   type VRAM_Type is new Memory_Mapped_Device with record
      Map : VRAM_Map_Type;
   end record;
   type VRAM_Access is access all VRAM_Type;

   overriding procedure Reset
     (VRAM : in out VRAM_Type);

   overriding procedure Read
     (VRAM    : in out VRAM_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte);

   overriding procedure Write
     (VRAM    : in out VRAM_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : Byte);

end Gade.Dev.VRAM;
