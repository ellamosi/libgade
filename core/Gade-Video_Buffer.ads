package Gade.Video_Buffer is

   type Color_Value is mod 4;

   type Color_Buffer is array
     (Natural range <>, Natural range <>) of Color_Value;

   Display_Width  : constant := 160;
   Display_Height : constant := 144;

   subtype Display_Horizontal_Range is Natural range 0 .. Display_Width - 1;
   subtype Display_Vertical_Range is Natural range 0 .. Display_Height - 1;

   type Video_Buffer_Type is new
      Color_Buffer (Display_Vertical_Range, Display_Horizontal_Range);

   type Video_Buffer_Access is access all Video_Buffer_Type;

   type Color_Component is range 0 .. 255 with
     Size       => 8,
     Convention => C;

   pragma Warnings (Off, "* bits of * unused");
   type RGB32_Color is
      record
         Red   : Color_Component;
         Green : Color_Component;
         Blue  : Color_Component;
      end record with
        Convention => C,
        Size       => Color_Component'Size * 4;
   pragma Warnings (On, "* bits of * unused");

   for RGB32_Color use
      record
         Red   at 0 range  0 ..  7;
         Green at 0 range  8 .. 15;
         Blue  at 0 range 16 .. 23;
      end record;

   type RGB32_Buffer is array
     (Natural range <>, Natural range <>) of RGB32_Color;

   type RGB32_Display_Buffer is new
     RGB32_Buffer (Display_Vertical_Range, Display_Horizontal_Range) with
     Convention => C;

   type RGB32_Display_Buffer_Access is access all RGB32_Display_Buffer with
     Convention => C;
   --  TODO: Figure out why the pragma is needed (release mode only)
   pragma No_Strict_Aliasing (RGB32_Display_Buffer_Access);

   Background_Width  : constant := 256;
   Background_Height : constant := 256;

   subtype Background_Horizontal_Range is Natural range 0 .. Background_Width - 1;
   subtype Background_Vertical_Range is Natural range 0 .. Background_Height - 1;

   type Background_Buffer_Type is new
      Color_Buffer (Background_Vertical_Range, Background_Horizontal_Range);

   type Background_Buffer_Access is access all Background_Buffer_Type;

   Tile_Buffer_Width  : constant := 128;
   Tile_Buffer_Height : constant := 128;

   subtype Tile_Buffer_Horizontal_Range is
      Natural range 0 .. Tile_Buffer_Width - 1;
   subtype Tile_Buffer_Vertical_Range is
      Natural range 0 .. Tile_Buffer_Height - 1;

   type Tile_Buffer_Type is new
      Color_Buffer (Tile_Buffer_Vertical_Range, Tile_Buffer_Horizontal_Range);

   type Tile_Buffer_Access is access all Tile_Buffer_Type;

end Gade.Video_Buffer;
