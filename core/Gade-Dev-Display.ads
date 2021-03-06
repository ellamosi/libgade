with Gade.Video_Buffer; use Gade.Video_Buffer;
with Gade.Dev.Video; use Gade.Dev.Video;
with Gade.Dev.Video.Sprites; use Gade.Dev.Video.Sprites;
limited private with Gade.Dev.Display.Handlers;

package Gade.Dev.Display is

   subtype Display_IO_Address is Word range 16#FF40# .. 16#FF4B#;

   type Display_Type is new Memory_Mapped_Device with private;

   procedure Create
     (Display : aliased out Display_Type);

   overriding
   procedure Reset
     (Display : in out Display_Type);

   overriding procedure Read
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte);

   overriding procedure Write
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : Byte);

   procedure Report_Cycles
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type;
      Video   : RGB32_Display_Buffer_Access;
      Cycles  : Positive);

   procedure Check_Frame_Finished
     (Display  : in out Display_Type;
      Finished : out Boolean);

   type Palette_Type is array (Color_Value'Range) of Color_Value;
   pragma Pack (Palette_Type);
   for Palette_Type'Size use 8;

   --  TODO: This type should go elsewhere?! (Public_Types?)
   type Palette_Info_Type is record
      BGP, OBP0, OBP1 : Palette_Type;
   end record;

private

   type Display_Handler_Access is access all Gade.Dev.Display.Handlers.Display_Handler_Type;

   --  FF40 - LCDCONT [RW] LCD Control
   --  TODO: Use more fitting types
   type LCD_Control is record
      --  Bit0  Background display                  | ON        | OFF
      Background_Display       : Boolean;
      --  Bit1  Sprite display                      | ON        | OFF
      Sprite_Display           : Boolean;
      --  Bit2  Sprite size                         | 8x16      | 8x8
      Sprite_Size              : Sprite_Size_Type;
      --  Bit3  Background Tile Map address         | 9C00-9FFF | 9800-9BFF
      Background_Tile_Map_Addr : Tile_Map_Access_Type;
      --  Bit4  Tile Data Table address             | 8000-8FFF | 8800-97FF
      Tile_Data_Table_Addr     : Tile_Data_Access_Type;
      --  Bit5  Window display                      | ON        | OFF
      Window_Display           : Boolean;
      --  Bit6  Window Tile Table address           | 9C00-9FFF | 9800-9BFF
      Window_Tile_Table_Addr   : Tile_Map_Access_Type;
      --  Bit7  LCD operation                       | ON        | OFF
      LCD_Operation            : Boolean;
   end record;
   for LCD_Control use record
      Background_Display         at 0 range 0 .. 0;
      Sprite_Display             at 0 range 1 .. 1;
      Sprite_Size                at 0 range 2 .. 2;
      Background_Tile_Map_Addr   at 0 range 3 .. 3;
      Tile_Data_Table_Addr       at 0 range 4 .. 4;
      Window_Display             at 0 range 5 .. 5;
      Window_Tile_Table_Addr     at 0 range 6 .. 6;
      LCD_Operation              at 0 range 7 .. 7;
   end record;

   Default_LCD_Control : constant LCD_Control :=
      (Background_Display         => True,
       Sprite_Display             => False,
       Sprite_Size                => Single,
       Background_Tile_Map_Addr   => Low_Map,
       Tile_Data_Table_Addr       => Low_Data,
       Window_Display             => False,
       Window_Tile_Table_Addr     => Low_Map,
       LCD_Operation              => True);

   type LCD_Controller_Mode_Type is (
      HBlank,
      --  Horizontal blanking impulse (VRAM 8000-9FFF can be accessed by CPU)
      VBlank,
      --  Vertical blanking impulse (VRAM 8000-9FFF can be accessed by CPU)
      OAM_Access,
      --  OAM FE00-FE90 is accessed by LCD controller
      VRAM_Access
      --  Both OAM FE00-FE90 and VRAM 8000-9FFF are accessed by LCD controller
   );
   for LCD_Controller_Mode_Type use
     (HBlank      => 2#00#,
      VBlank      => 2#01#,
      OAM_Access  => 2#10#,
      VRAM_Access => 2#11#);

   Starting_Line : constant := 144; -- Start of VBlank, THIS IS MADE UP!
   Starting_Mode : constant LCD_Controller_Mode_Type := VBlank;

   --  FF41 -- LCDSTAT [RW] LCD Status
   --  TODO: Use more fitting types
   type LCD_Status is record
      --  Bit1-0  LCD Controller mode
      LCD_Controller_Mode            : LCD_Controller_Mode_Type;
      --  Bit2    Scanline coincidence flag         | COINCIDENCE   | NO COINCIDENCE
      Scanline_Coincidence           : Boolean;
      --  Bit3    Interrupt on HBlank (On/Off)
      Interrupt_HBlank               : Boolean;
      --  Bit4    Interrupt on VBlank (On/Off)
      Interrupt_VBlank               : Boolean;
      --  Bit5    Interrupt on OAM Access (On/Off)
      Interrupt_OAM_Access           : Boolean;
      --  Bit6    Interrupt on scanline coincidence (On/Off)
      Interrupt_Scanline_Coincidence : Boolean;
   end record;
   for LCD_Status use record
      LCD_Controller_Mode            at 0 range 0 .. 1;
      Scanline_Coincidence           at 0 range 2 .. 2;
      Interrupt_HBlank               at 0 range 3 .. 3;
      Interrupt_VBlank               at 0 range 4 .. 4;
      Interrupt_OAM_Access           at 0 range 5 .. 5;
      Interrupt_Scanline_Coincidence at 0 range 6 .. 6;
      --  Reserved bit
   end record;
   for LCD_Status'Size use 8;

   Default_LCD_Status : constant LCD_Status :=
      (LCD_Controller_Mode            => Starting_Mode,
       Scanline_Coincidence           => False,
       Interrupt_HBlank               => False,
       Interrupt_VBlank               => False,
       Interrupt_OAM_Access           => False,
       Interrupt_Scanline_Coincidence => False);

   type Color_Palette is mod 2**2;
   --  00 ------- 01 ------- 10 -------> 11
   --  lightest                     darkest

   type LCD_Address_Space is array (Display_IO_Address'Range) of Byte;

   subtype Line_Count_Type is Natural range 0 .. 153;

   type LCD_Access_Type is (Named, Address);
   type LCD_Map_Type (Access_Type : LCD_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            --  FF40 -- LCDCONT [RW] LCD Control
            LCDC    : LCD_Control;
            --  FF41 -- LCDSTAT [RW] LCD Status
            STAT    : LCD_Status;
            --  FF42 -- SCROLLY [RW] Background Vertical Scrolling
            SCROLLY : Byte;
            --  FF43 -- SCROLLX [RW] Background Horizontal Scrolling
            SCROLLX : Byte;
            --  FF44 -- CURLINE [RW] Current Scanline
            --  This register contains the number of a screen line currently
            --  being scanned. It can take values 0-153 where 144-153
            --  indicate the vertical blanking period. Writing into this
            --  register resets it.
            CURLINE : Line_Count_Type;
            --  FF45 -- CMPLINE [RW] Scanline Comparison
            --  When contents of CURLINE are equal to contents of CMPLINE,
            --  scanline coincidence flag is set in the LCD status register
            --  and an interrupt may occur.
            CMPLINE : Byte;
            --  FF46 -- OAM DMA
            DMA : Byte;
            --  FF47 -- BGRDPAL [W] Background/Window Palette
            BGRDPAL : Palette_Type;
            --  FF48 -- OBJ0PAL [W] Sprite Palette #0
            OBJ0PAL : Palette_Type;
            --  FF49 -- OBJ1PAL [W] Sprite Palette #1
            OBJ1PAL : Palette_Type;
            --  FF4A -- WNDPOSY [RW] Window Y Position
            --  WNDPOSY may assume values 0-143. It determines the vertical
            --  position of the left upper corner of a window on the screen.
            WNDPOSY : Byte;
            --  FF4B -- WNDPOSX [RW] Window X Position
            --  WNDPOSX may assume values 7-166. It determines the horizontal
            --  position of the left upper corner of a window on the screen.
            --  The real position is WNDPOSX-7.
            WNDPOSX : Byte;
         when Address =>
            Space   : LCD_Address_Space;
      end case;
   end record with Unchecked_Union;
   for LCD_Map_Type use record
      LCDC    at  0 range 0 .. 7;
      STAT    at  1 range 0 .. 7;
      SCROLLY at  2 range 0 .. 7;
      SCROLLX at  3 range 0 .. 7;
      CURLINE at  4 range 0 .. 7;
      CMPLINE at  5 range 0 .. 7;
      DMA     at  6 range 0 .. 7;
      BGRDPAL at  7 range 0 .. 7;
      OBJ0PAL at  8 range 0 .. 7;
      OBJ1PAL at  9 range 0 .. 7;
      WNDPOSY at 10 range 0 .. 7;
      WNDPOSX at 11 range 0 .. 7;
   end record;

   type Display_Type is
     new Memory_Mapped_Device with record
      Frame_Finished : Boolean;
      DMA_Source_Address : Word;
      DMA_Target_Address : Word;
      DMA_Clocks_Since_Last_Copy : Integer; -- 1/4 clocks + 4 clocks setup
      DMA_Copy_Ongoing : Boolean;
      Map : LCD_Map_Type;
      Display_Handler : Display_Handler_Access;
   end record;

   type Display_Access is access all Display_Type;

--  ------------------------------------------------------------------------------
--  FF46 -- DMACONT [W] DMA Transfer Control
--          Writing to this register will cause a DMA transfer into OAM located
--          at FE00-FE9F. The written value determines the source address in a
--          following way: 00 -> 0000, 01 -> 0100, ... , 9A -> 9A00, ...
--          The DMA transfer takes about 160 nanoseconds.
--
--          Example:
--          ; Routine transferring 0400-049F into OAM
--          DI           ; Disable interrupts
--          LD A,04h     ; Transferring data from 0400h
--          LD (FF46h),A ; Start DMA transfer
--    LOOP: LD A,#40     ; Wait
--          DEC A        ;
--          JR NZ,LOOP   ;
--          EI           ; Enable interrupts
--

   Color_Lookup : constant array (Color_Value'Range) of RGB32_Color :=
     ((255, 255, 255), (171, 171, 171), (85, 85, 85), (0, 0, 0));

   procedure Do_DMA
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type);

   procedure Line_Changed
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type;
      Line    : Line_Count_Type);

   procedure Mode_Changed
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type;
      Mode    : LCD_Controller_Mode_Type);

end Gade.Dev.Display;

