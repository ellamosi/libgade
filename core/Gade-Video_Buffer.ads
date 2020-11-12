package Gade.Video_Buffer is

   --  59.73 Hz, adjusting unit for integer precision sake:
   Frame_Frequency : constant := 5973; -- cHz

   Display_Width  : constant := 160;
   Display_Height : constant := 144;

   subtype Display_Horizontal_Range is Natural range 0 .. Display_Width - 1;
   subtype Display_Vertical_Range is Natural range 0 .. Display_Height - 1;

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

   type RGB32_Display_Buffer is array
     (Display_Vertical_Range, Display_Horizontal_Range) of RGB32_Color with
     Convention => C;

   type RGB32_Display_Buffer_Access is access all RGB32_Display_Buffer with
     Convention => C;
   pragma No_Strict_Aliasing (RGB32_Display_Buffer_Access);

end Gade.Video_Buffer;
