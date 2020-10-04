package Gade.Audio_Buffer is

   --  Gambatte:
   --  There are 35112 audio (stereo) samples in a video frame.
   --  An audio sample consists of two native endian 2s complement 16-bit PCM samples
   --  May run for up to 2064 audio samples too long.

   --  Own draft:
   --  35112 samples/frame at ~59.7 fps => 2096186.4 samples/sec (~2096kHz)
   --

   Sample_Bit_Size : constant := 16;
   Sample_Minimum  : constant := -(2 ** (Sample_Bit_Size - 1));
   Sample_Maximum  : constant := 2 ** (Sample_Bit_Size - 1) - 1;

   type Sample is range Sample_Minimum .. Sample_Maximum;
   for Sample'Size use Sample_Bit_Size;

   type Stereo_Sample is record
      Left, Right : Sample;
   end record;

   type Audio_Buffer is array (Natural range <>) of Stereo_Sample;

   Samples_Frame   : constant := 35_112; -- / 2;
   --  Samples_Second  : constant := Samples_Frame * 60; -- 59.727500569606
   Samples_Second  : constant := 2_097_152 / 2;
   Extra_Samples   : constant := 2_064; -- May run for some samples too long
   Maximum_Samples : constant := Samples_Frame + Extra_Samples;

   type Audio_Buffer_Type is new Audio_Buffer (0 .. Maximum_Samples - 1);
   --  with Convention => C;

   type Audio_Buffer_Access is access all Audio_Buffer_Type;
   --  with Convention => C;

--
--     type Color_Value is mod 4;
--
--     type Color_Buffer is array
--       (Natural range <>, Natural range <>) of Color_Value;
--
--     Display_Width  : constant := 160;
--     Display_Height : constant := 144;
--
--     subtype Display_Horizontal_Range is Natural range 0 .. Display_Width - 1;
--     subtype Display_Vertical_Range is Natural range 0 .. Display_Height - 1;
--
--     type Video_Buffer_Type is new
--        Color_Buffer (Display_Vertical_Range, Display_Horizontal_Range);
--
--     type Video_Buffer_Access is access all Video_Buffer_Type;
--
--     type Color_Component is range 0 .. 255 with
--       Size       => 8,
--       Convention => C;
--
--     pragma Warnings (Off, "* bits of * unused");
--     type RGB32_Color is
--        record
--           Red   : Color_Component;
--           Green : Color_Component;
--           Blue  : Color_Component;
--        end record with
--          Convention => C,
--          Size       => Color_Component'Size * 4;
--     pragma Warnings (On, "* bits of * unused");
--
--     for RGB32_Color use
--        record
--           Red   at 0 range  0 ..  7;
--           Green at 0 range  8 .. 15;
--           Blue  at 0 range 16 .. 23;
--        end record;
--
--     type RGB32_Buffer is array
--       (Natural range <>, Natural range <>) of RGB32_Color;
--
--     type RGB32_Display_Buffer is new
--       RGB32_Buffer (Display_Vertical_Range, Display_Horizontal_Range) with
--       Convention => C;
--
--     type RGB32_Display_Buffer_Access is access all RGB32_Display_Buffer with
--       Convention => C;
--
--     Background_Width  : constant := 256;
--     Background_Height : constant := 256;
--
--     subtype Background_Horizontal_Range is Natural range 0 .. Background_Width - 1;
--     subtype Background_Vertical_Range is Natural range 0 .. Background_Height - 1;
--
--     type Background_Buffer_Type is new
--        Color_Buffer (Background_Vertical_Range, Background_Horizontal_Range);
--
--     type Background_Buffer_Access is access all Background_Buffer_Type;
--
--     Tile_Buffer_Width  : constant := 128;
--     Tile_Buffer_Height : constant := 128;
--
--     subtype Tile_Buffer_Horizontal_Range is
--        Natural range 0 .. Tile_Buffer_Width - 1;
--     subtype Tile_Buffer_Vertical_Range is
--        Natural range 0 .. Tile_Buffer_Height - 1;
--
--     type Tile_Buffer_Type is new
--        Color_Buffer (Tile_Buffer_Vertical_Range, Tile_Buffer_Horizontal_Range);
--
--     type Tile_Buffer_Access is access all Tile_Buffer_Type;

end Gade.Audio_Buffer;
