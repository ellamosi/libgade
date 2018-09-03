limited with Gade.Cart.Spaces.ROM;
limited with Gade.Cart.Spaces.RAM;
private with Gade.Cartridge_Info;

private package Gade.Cart is

   type Cart_Type is
     (ROM_ONLY,                  -- 0
      ROM_MBC1,                  -- 1
      ROM_MBC1_RAM,              -- 2
      ROM_MBC1_RAM_BATT,         -- 3
      ROM_MBC2,                  -- 5
      ROM_MBC2_BATT,             -- 6
      ROM_RAM,                   -- 8
      ROM_RAM_BATT,              -- 9
      ROM_MM01,                  -- B
      ROM_MM01_SRAM,             -- C
      ROM_MM01_SRAM_BATT,        -- D
      ROM_MBC3_TIMER_BATT,       -- F
      ROM_MBC3_TIMER_RAM_BATT,   -- 10
      ROM_MBC3,                  -- 11
      ROM_MBC3_RAM,              -- 12
      ROM_MBC3_RAM_BATT,         -- 13
      ROM_MBC5,                  -- 19
      ROM_MBC5_RAM,              -- 1A
      ROM_MBC5_RAM_BATT,         -- 1B
      ROM_MBC5_RUMBLE,           -- 1C
      ROM_MBC5_RUMBLE_SRAM,      -- 1D
      ROM_MBC5_RUMBLE_SRAM_BATT, -- 1E
      Pocket_Camera,             -- FC
      Bandai_TAMA5,              -- FD
      Huds_on_Huc_3,             -- FE
      Huds_on_Huc_1);            -- FF

   --  0100- 0103 This is  he begin code execution point in a cart. Usually
   --  there is a NOP and a JP instruction here but not always.
   type Entry_Point_Type is array (0 .. 3) of Byte;

   --  0104- 0133 Scrolling Brand Logo
   type Scrolling_Logo_Type is array (0 .. 16#2F#) of Byte;

   --  0134- 0142 Title of the game in UPPER CASE ASCII. If it is less than 16
   --  characters then the remaining bytes are filled with 00's.
   type Game_Title_Type is array (0 .. 14) of Byte; -- Revise type

   --  0143 $80 = Color GB, $00 or other = not Color GB
   --  0144 Ascii hex digit, high nibble of licensee code (new).
   --  0145 Ascii hex digit, low nibble of licensee code (new). (These are
   --  normally $00 if [$014B] <> $33.)
   --  0146 GB SGB Indicator (00 = GameBoy, 03 = Super GameBoy functions)
   --  (Super GameBoy functions won't work if <> $03.)
   --  0147 Cartridge type

   --  0148 ROM size:
   --  0 - 256Kbit = 32KByte = 2 banks
   --  1 - 512Kbit = 64KByte = 4 banks
   --  2 - 1Mbit = 128KByte = 8 banks
   --  3 - 2Mbit = 256KByte = 16 banks
   --  4 - 4Mbit = 512KByte = 32 banks
   --  5 - 8Mbit    = 1MByte   = 64 banks
   --  6   - 16Mbit = 2MByte   = 128 banks
   --  $52 - 9Mbit  = 1.1MByte = 72 banks
   --  $53 - 10Mbit = 1.2MByte = 80 banks
   --  $54 - 12Mbit = 1.5MByte = 96 banks
   type ROM_Size_Type is
     (ROM_256kbit,
      ROM_512kbit,
      ROM_1Mbit,
      ROM_2Mbit,
      ROM_4Mbit,
      ROM_8Mbit,
      ROM_16Mbit,
      ROM_32Mbit,
      ROM_64Mbit,
      ROM_9Mbit,
      ROM_10Mbit,
      ROM_12Mbit);

   --  0149 RAM size:
   --  0 -    None (Including MBC2 built-in RAM)
   --  1 -  16kBit =   2kByte =  1 x 2kByte bank  (MBC1, MBC5)
   --  2 -  64kBit =   8kByte =  1 x 8kByte banks (MBC1, MBC3, MBC5)
   --  3 - 256kBit =  32kByte =  4 x 8kByte banks (MBC1, MBC3, MBC5)
   --  4 -   1MBit = 128kByte = 16 x 8kByte banks (MBC5)
   --  5 - 512kBit =  64kByte =  8 x 8kByte banks (MBC30, MBC5)
   type RAM_Size_Type is
     (None,
      RAM_16kbit,
      RAM_64kbit,
      RAM_256kbit,
      RAM_1Mbit,
      RAM_512kbit);

   --  014A Destination code:
   --  0 - Japanese
   --  1 - Non-Japanese
   type Destination_Type is (Japanese, Non_Japanese);

   --  014B Licensee code (old):
   --  33 - Check 0144/0145 for Licensee code.
   --  79 - Accolade
   --  A4 - Konami
   --  (Super GameBoy function won't work if <> $33.)

   --  014C Mask ROM Version number (Usually $00)

   --  014D Complement check
   --  (PROGRAM WON'T RUN ON GB IF NOT CORRECT!!!)
   --  (It will run on Super GB, however, if incorrect.)

   --  014E-014F Checksum (higher byte first) produced by adding all bytes of a
   --  cartridge except for two checksum bytes and taking two lower bytes of
   --  the result. (GameBoy ignores this value.)

   type Cart_Header is record
      Entry_Point        : Entry_Point_Type;
      Scrolling_Logo     : Scrolling_Logo_Type;
      Game_Title         : Game_Title_Type;
      GB_Color           : Byte; -- Todo: Look into this
      Licensee_Code_Low  : Byte; -- Todo: Look into this
      Licensee_Code_High : Byte; -- Todo: Look into this
      Super_GB           : Byte; -- Todo: Look into this
      Cart_Type          : Gade.Cart.Cart_Type;
      ROM_Size           : ROM_Size_Type;
      RAM_Size           : RAM_Size_Type;
      Destination        : Destination_Type;
      Licensee_Code_Old  : Byte; -- Todo: Look into this
      Mask_ROM_Version   : Byte; -- Todo: Look into this
      Complement_Check   : Byte;
      Checksum           : Word;
   end record;
   type Cart_Header_Access is access Cart_Header;

   procedure Load_ROM
     (ROM_Handler : out Spaces.ROM.Handler_Access;
      RAM_Handler : out Spaces.RAM.Handler_Access;
      Path        : String);

private
   use Gade.Cartridge_Info;

   for Cart_Type use
     (ROM_ONLY                  => 16#00#,
      ROM_MBC1                  => 16#01#,
      ROM_MBC1_RAM              => 16#02#,
      ROM_MBC1_RAM_BATT         => 16#03#,
      ROM_MBC2                  => 16#05#,
      ROM_MBC2_BATT             => 16#06#,
      ROM_RAM                   => 16#08#,
      ROM_RAM_BATT              => 16#09#,
      ROM_MM01                  => 16#0B#,
      ROM_MM01_SRAM             => 16#0C#,
      ROM_MM01_SRAM_BATT        => 16#0D#,
      ROM_MBC3_TIMER_BATT       => 16#0F#,
      ROM_MBC3_TIMER_RAM_BATT   => 16#10#,
      ROM_MBC3                  => 16#11#,
      ROM_MBC3_RAM              => 16#12#,
      ROM_MBC3_RAM_BATT         => 16#13#,
      ROM_MBC5                  => 16#19#,
      ROM_MBC5_RAM              => 16#1A#,
      ROM_MBC5_RAM_BATT         => 16#1B#,
      ROM_MBC5_RUMBLE           => 16#1C#,
      ROM_MBC5_RUMBLE_SRAM      => 16#1D#,
      ROM_MBC5_RUMBLE_SRAM_BATT => 16#1E#,
      Pocket_Camera             => 16#FC#,
      Bandai_TAMA5              => 16#FD#,
      Huds_on_Huc_3             => 16#FE#,
      Huds_on_Huc_1             => 16#FF#);

   for ROM_Size_Type use
     (ROM_256kbit => 16#00#,
      ROM_512kbit => 16#01#,
      ROM_1Mbit   => 16#02#,
      ROM_2Mbit   => 16#03#,
      ROM_4Mbit   => 16#04#,
      ROM_8Mbit   => 16#05#,
      ROM_16Mbit  => 16#06#,
      ROM_32Mbit  => 16#07#,
      ROM_64Mbit  => 16#08#,
      ROM_9Mbit   => 16#52#,
      ROM_10Mbit  => 16#53#,
      ROM_12Mbit  => 16#54#);

   for RAM_Size_Type use
     (None        => 0,
      RAM_16kbit  => 1,
      RAM_64kbit  => 2,
      RAM_256kbit => 3,
      RAM_1Mbit   => 4,
      RAM_512kbit => 5);

   for Destination_Type use (Japanese => 0, Non_Japanese => 1);

   for Cart_Header use record
      Entry_Point        at 16#100# range 0 .. 31;
      Scrolling_Logo     at 16#104# range 0 .. 8 * 16#30# - 1;
      Game_Title         at 16#134# range 0 .. 8 * 16#0F# - 1;
      GB_Color           at 16#143# range 0 .. 7;
      Licensee_Code_Low  at 16#144# range 0 .. 7;
      Licensee_Code_High at 16#145# range 0 .. 7;
      Super_GB           at 16#146# range 0 .. 7;
      Cart_Type          at 16#147# range 0 .. 7;
      ROM_Size           at 16#148# range 0 .. 7;
      RAM_Size           at 16#149# range 0 .. 7;
      Destination        at 16#14A# range 0 .. 7;
      Licensee_Code_Old  at 16#14B# range 0 .. 7;
      Mask_ROM_Version   at 16#14C# range 0 .. 7;
      Complement_Check   at 16#14D# range 0 .. 7;
      Checksum           at 16#14E# range 0 .. 15;
   end record;
   for Cart_Header'Size use 16#150# * 8;

   Controller_Type_For_Cart : constant array (Cart_Type)
     of Controller_Type :=
       (ROM_ONLY                  => None,
        ROM_MBC1                  => MBC1,
        ROM_MBC1_RAM              => MBC1,
        ROM_MBC1_RAM_BATT         => MBC1,
        ROM_MBC2                  => MBC2,
        ROM_MBC2_BATT             => MBC2,
        ROM_RAM                   => None,
        ROM_RAM_BATT              => None,
        ROM_MM01                  => MM01,
        ROM_MM01_SRAM             => MM01,
        ROM_MM01_SRAM_BATT        => MM01,
        ROM_MBC3_TIMER_BATT       => MBC3,
        ROM_MBC3_TIMER_RAM_BATT   => MBC3,
        ROM_MBC3                  => MBC3,
        ROM_MBC3_RAM              => MBC3,
        ROM_MBC3_RAM_BATT         => MBC3,
        ROM_MBC5                  => MBC5,
        ROM_MBC5_RAM              => MBC5,
        ROM_MBC5_RAM_BATT         => MBC5,
        ROM_MBC5_RUMBLE           => MBC5,
        ROM_MBC5_RUMBLE_SRAM      => MBC5,
        ROM_MBC5_RUMBLE_SRAM_BATT => MBC5,
        Pocket_Camera             => Pocket_Camera,
        Bandai_TAMA5              => Bandai_TAMA5,
        Huds_on_Huc_3             => Huds_on_Huc_3,
        Huds_on_Huc_1             => Huds_on_Huc_1);

   type RAM_Handler_Kind_Type is
     (None, MBC1);

   RAM_Handler_Kind_For_Cart : constant array (Cart_Type)
     of RAM_Handler_Kind_Type :=
       (ROM_ONLY                  => None,
        ROM_MBC1                  => None,
        ROM_MBC1_RAM              => MBC1,
        ROM_MBC1_RAM_BATT         => MBC1,
        ROM_MBC2                  => None,
        ROM_MBC2_BATT             => None,
        ROM_RAM                   => MBC1, -- For now
        ROM_RAM_BATT              => MBC1, -- For now
        ROM_MM01                  => None,
        ROM_MM01_SRAM             => None,
        ROM_MM01_SRAM_BATT        => None,
        ROM_MBC3_TIMER_BATT       => None,
        ROM_MBC3_TIMER_RAM_BATT   => None,
        ROM_MBC3                  => None,
        ROM_MBC3_RAM              => None,
        ROM_MBC3_RAM_BATT         => None,
        ROM_MBC5                  => None,
        ROM_MBC5_RAM              => None,
        ROM_MBC5_RAM_BATT         => None,
        ROM_MBC5_RUMBLE           => None,
        ROM_MBC5_RUMBLE_SRAM      => None,
        ROM_MBC5_RUMBLE_SRAM_BATT => None,
        Pocket_Camera             => None,
        Bandai_TAMA5              => None,
        Huds_on_Huc_3             => None,
        Huds_on_Huc_1             => None);

end Gade.Cart;
