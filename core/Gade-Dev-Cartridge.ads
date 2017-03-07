limited with Gade.GB;

package Gade.Dev.Cartridge is
   -- Only the ROM part of the cartridge, should be renamed (External_ROM?)

   subtype External_ROM_IO_Address is Word range 16#0000#..16#7FFF#;

   type External_ROM_Type is new Memory_Mapped_Device with private;

   procedure Reset (External_ROM : in out External_ROM_Type);

   procedure Load_ROM
     (External_ROM : out External_ROM_Type;
      Path         : String);

   procedure Read
     (External_ROM : in out External_ROM_Type;
      GB           : in out Gade.GB.GB_Type;
      Address      : Word;
      Content      : out Byte);

   procedure Write
     (External_ROM : in out External_ROM_Type;
      GB           : in out Gade.GB.GB_Type;
      Address      : Word;
      Content      : Byte);

private

   type Cartridge_Type is
     (ROM_ONLY,                  --0
      ROM_MBC1,                  --1
      ROM_MBC1_RAM,              --2
      ROM_MBC1_RAM_BATT,         --3
      ROM_MBC2,                  --5
      ROM_MBC2_BATT,             --6
      ROM_RAM,                   --8
      ROM_RAM_BATT,              --9
      ROM_MM01,                  --B
      ROM_MM01_SRAM,             --C
      ROM_MM01_SRAM_BATT,        --D
      ROM_MBC3_TIMER_BATT,       --F
      ROM_MBC3_TIMER_RAM_BATT,   --10
      ROM_MBC3,                  --11
      ROM_MBC3_RAM,              --12
      ROM_MBC3_RAM_BATT,         --13
      ROM_MBC5,                  --19
      ROM_MBC5_RAM,              --1A
      ROM_MBC5_RAM_BATT,         --1B
      ROM_MBC5_RUMBLE,           --1C
      ROM_MBC5_RUMBLE_SRAM,      --1D
      ROM_MBC5_RUMBLE_SRAM_BATT, --1E
      Pocket_Camera,             --1F
      Bandai_TAMA5,              --FD
      Huds_on_Huc_3,             --FE
      Huds_on_Huc_1              --FF
     );
   for Cartridge_Type use
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
      Pocket_Camera             => 16#1F#,
      Bandai_TAMA5              => 16#FD#,
      Huds_on_Huc_3             => 16#FE#,
      Huds_on_Huc_1             => 16#FF#
     );
   for Cartridge_Type'Size use 8;

   -- 0100- 0103 This is  he begin code execution point in a cart. Usually
   -- there is a NOP and a JP instruction here but not always.
   type Entry_Point_Type is array (0..3) of Byte;

   -- 0104- 0133 Scrolling Brand Logo
   type Scrolling_Logo_Type is array (0..16#2F#) of Byte;

   -- 0134- 0142 Title of the game in UPPER CASE ASCII. If it is less than 16
   -- characters then the remaining bytes are filled with 00's.
   type Game_Title_Type is array (0..14) of Byte; -- Revise type

   -- 0143 $80 = Color GB, $00 or other = not Color GB
   -- 0144 Ascii hex digit, high nibble of licensee code (new).
   -- 0145 Ascii hex digit, low nibble of licensee code (new). (These are
   -- normally $00 if [$014B] <> $33.)
   -- 0146 GB SGB Indicator (00 = GameBoy, 03 = Super GameBoy functions)
   -- (Super GameBoy functions won't work if <> $03.)
   -- 0147 Cartridge type

   -- 0148 ROM size:
   -- 0 - 256Kbit = 32KByte = 2 banks
   -- 1 - 512Kbit = 64KByte = 4 banks
   -- 2 - 1Mbit = 128KByte = 8 banks
   -- 3 - 2Mbit = 256KByte = 16 banks
   -- 4 - 4Mbit = 512KByte = 32 banks
   -- 5 - 8Mbit    = 1MByte   = 64 banks
   -- 6   - 16Mbit = 2MByte   = 128 banks
   -- $52 - 9Mbit  = 1.1MByte = 72 banks
   -- $53 - 10Mbit = 1.2MByte = 80 banks
   -- $54 - 12Mbit = 1.5MByte = 96 banks
   type ROM_Size_Type is
     (ROM_256kbit,
      ROM_512kbit,
      ROM_1Mbit,
      ROM_2Mbit,
      ROM_4Mbit,
      ROM_8Mbit,
      ROM_16Mbit,
      ROM_9Mbit,
      ROM_10Mbit,
      ROM_12Mbit);
   for ROM_Size_Type use
     (ROM_256kbit => 16#00#,
      ROM_512kbit => 16#01#,
      ROM_1Mbit   => 16#02#,
      ROM_2Mbit   => 16#03#,
      ROM_4Mbit   => 16#04#,
      ROM_8Mbit   => 16#05#,
      ROM_16Mbit  => 16#06#,
      ROM_9Mbit   => 16#52#,
      ROM_10Mbit  => 16#53#,
      ROM_12Mbit  => 16#54#);
   for ROM_Size_Type'Size use 8;

   ROM_Bank_Count : constant array(ROM_Size_Type) of Integer :=
     (ROM_256kbit =>   2,
      ROM_512kbit =>   4,
      ROM_1Mbit   =>   8,
      ROM_2Mbit   =>  16,
      ROM_4Mbit   =>  32,
      ROM_8Mbit   =>  64,
      ROM_16Mbit  => 128,
      ROM_9Mbit   =>  72,
      ROM_10Mbit  =>  80,
      ROM_12Mbit  =>  96);

   -- 0149 RAM size:
   -- 0 - None
   -- 1 - 16kBit  = 2kB   =  1 bank
   -- 2 - 64kBit  = 8kB   =  1 bank
   -- 3 - 256kBit = 32kB  =  4 banks
   -- 4 - 1MBit   = 128kB = 16 banks
   type RAM_Size_Type is
     (None,
      RAM_16kbit,
      RAM_64kbit,
      RAM_256kbit,
      RAM_1Mbit);
   for RAM_Size_Type use
     (None        => 0,
      RAM_16kbit  => 1,
      RAM_64kbit  => 2,
      RAM_256kbit => 3,
      RAM_1Mbit   => 4);
   for RAM_Size_Type'Size use 8;

   -- 014A Destination code:
   -- 0 - Japanese
   -- 1 - Non-Japanese
   type Destination_Type is (Japanese, Non_Japanese);
   for Destination_Type use (Japanese => 0, Non_Japanese => 1);
   for Destination_Type'Size use 8;

   -- 014B Licensee code (old):
   -- 33 - Check 0144/0145 for Licensee code.
   -- 79 - Accolade
   -- A4 - Konami
   -- (Super GameBoy function won't work if <> $33.)

   -- 014C Mask ROM Version number (Usually $00)

   -- 014D Complement check
   -- (PROGRAM WON'T RUN ON GB IF NOT CORRECT!!!)
   -- (It will run on Super GB, however, if incorrect.)

   -- 014E-014F Checksum (higher byte first) produced by adding all bytes of a
   -- cartridge except for two checksum bytes and taking two lower bytes of
   -- the result. (GameBoy ignores this value.)

   type Cartridge_Info is record
      Entry_Point        : Entry_Point_Type;
      Scrolling_Logo     : Scrolling_Logo_Type;
      Game_Title         : Game_Title_Type;
      GB_Color           : Byte; -- Todo: Look into this
      Licensee_Code_Low  : Byte; -- Todo: Look into this
      Licensee_Code_High : Byte; -- Todo: Look into this
      Super_GB           : Byte; -- Todo: Look into this
      Cartridge_Type     : Gade.Dev.Cartridge.Cartridge_Type;
      ROM_Size           : ROM_Size_Type;
      RAM_Size           : RAM_Size_Type;
      Destination        : Destination_Type;
      Licensee_Code_Old  : Byte; -- Todo: Look into this
      Mask_ROM_Version   : Byte; -- Todo: Look into this
      Complement_Check   : Byte;
      Checksum           : Word;
   end record;
   for Cartridge_Info use record
      Entry_Point        at 16#00# range 0..31;
      Scrolling_Logo     at 16#04# range 0..8*16#30# - 1;
      Game_Title         at 16#34# range 0..8*16#0F# - 1;
      GB_Color           at 16#43# range 0..7;
      Licensee_Code_Low  at 16#44# range 0..7;
      Licensee_Code_High at 16#45# range 0..7;
      Super_GB           at 16#46# range 0..7;
      Cartridge_Type     at 16#47# range 0..7;
      ROM_Size           at 16#48# range 0..7;
      RAM_Size           at 16#49# range 0..7;
      Destination        at 16#4A# range 0..7;
      Licensee_Code_Old  at 16#4B# range 0..7;
      Mask_ROM_Version   at 16#4C# range 0..7;
      Complement_Check   at 16#4D# range 0..7;
      Checksum           at 16#4E# range 0..15;
   end record;
   for Cartridge_Info'Size use 16#50#*8;

   type Controller_Type is
     (None,
      MBC1,
      MBC2,
      MBC3,
      MBC5,
      MBC6,
      MBC7,
      MM01,
      Pocket_Camera,
      Bandai_TAMA5,
      Huds_on_Huc_1,
      Huds_on_Huc_3);

   subtype Supported_Controller_Type is Controller_Type range None .. MBC2;

   Controller_Type_For_Cartridge : constant array(Cartridge_Type)
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
      Huds_on_Huc_1             => Huds_on_Huc_1
     );

   type MBC1_Mem_Model_Type is
     (Mode_16_8, -- ROM Banking Mode
      Mode_4_32  -- RAM Banking Mode
     );

   subtype Bank_Address_Range is Word range 0..16*1024-1; -- Bank size is 16kB
   type ROM_Bank_Address_Space is array (Bank_Address_Range) of Byte;

   type ROM_Bank_Access_Type is (Named, Address);
   type ROM_Bank_Type (Access_Type : ROM_Bank_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            Cartridge_Info : Cartridge.Cartridge_Info;
         when Address =>
            Space : ROM_Bank_Address_Space;
      end case;
   end record;
   for ROM_Bank_Type use record
      Cartridge_Info at 16#100# range 0..16#50#*8-1;
   end record;
   pragma Unchecked_Union (ROM_Bank_Type);

   type ROM_Bank_Access is access ROM_Bank_Type;

   type ROM_Banks_Range is range 0..127;
   type ROM_Banks is array (ROM_Banks_Range) of ROM_Bank_Access;

   type External_ROM_Type is new Memory_Mapped_Device with record
      Current_Bank_1     : ROM_Bank_Access;
      Banks              : ROM_Banks;
      Controller         : Controller_Type;
      MBC1_Mem_Model     : MBC1_Mem_Model_Type;
      Current_Bank_1_Idx : ROM_Banks_Range;
   end record;

end Gade.Dev.Cartridge;
