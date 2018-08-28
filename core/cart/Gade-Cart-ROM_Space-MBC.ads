with Gade.Cart.RAM_Space;

package Gade.Cart.ROM_Space.MBC is

   --  ROM Reading

   --  MBC1: 0000-3FFF - ROM Bank 00 (Read Only)
   --  MBC2: 0000-3FFF - ROM Bank 00 (Read Only)
   --  MBC3: 0000-3FFF - ROM Bank 00 (Read Only)
   --  MBC5: 0000-3FFF - ROM Bank 00 (Read Only)

   --  MBC1: 4000-7FFF - ROM Bank 01-7F (Read Only) banks 20h, 40h, 60h not supported
   --  MBC2: 4000-7FFF - ROM Bank 01-0F (Read Only) only 16 banks
   --  MBC3: 4000-7FFF - ROM Bank 01-7F (Read Only) banks 20h, 40h, 60h supported
   --  MBC5: 4000-7FFF - ROM Bank 00-1FF (Read Only)

   --  RAM Reading/Writing

   --  MBC1: A000-BFFF - RAM Bank 00-03, if any (Read/Write)
   --  MBC2: A000-A1FF - 512x4bits RAM, built-in into the MBC2 chip (Read/Write)
   --  MBC3: A000-BFFF - RAM Bank 00-03, if any (Read/Write)
   --  MBC5: A000-BFFF - RTC Register 08-0C (Read/Write)

   --  RAM Enable

   --  MBC1: 0000-1FFF - RAM Enable (Write Only) (Value based)
   --  MBC2: 0000-1FFF - RAM Enable (Write Only) (Address based)
   --  MBC3: 0000-1FFF - RAM and Timer Enable (Write Only) (Value based, always affects timer)
   --  MBC5: 0000-1FFF - RAM Enable (Write Only) (Value based)

   --  ROM Bank Select

   --  MBC1: 2000-3FFF - ROM Bank Number (Write Only) 0, 20h, 40h, 60h are mapped to +1
   --        4000-5FFF - RAM Bank Number - or - Upper Bits of ROM Bank Number (Write Only) NOOOOOOOOOOOOO
   --  MBC2: 2000-3FFF - ROM Bank Number (Write Only) only certain addresses work!
   --  MBC3: 2000-3FFF - ROM Bank Number (Write Only) 0 maps to 1
   --  MBC5: 2000-2FFF - Low 8 bits of ROM Bank Number (Write Only)
   --        3000-3FFF - High bit of ROM Bank Number (Write Only)

   --  RAM Bank Select

   --  MBC1: 4000-5FFF - RAM Bank Number - or - Upper Bits of ROM Bank Number (Write Only)
   --  MBC2: -
   --  MBC3: 4000-5FFF - RAM Bank Number - or - RTC Register Select (Write Only)
   --  MBC5: 4000-5FFF - RAM Bank Number (Write Only)

   --  Special

   --  MBC1: 6000-7FFF - ROM/RAM Mode Select (Write Only)
   --  MBC2: -
   --  MBC3: 6000-7FFF - Latch Clock Data (Write Only)
   --  MBC5: -

   type MBC_ROM_Space_Type is abstract new ROM_Space_Type with private;

   overriding
   procedure Write
     (Space   : in out MBC_ROM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte);

private
   use Gade.Cart.RAM_Space;

   type MBC_ROM_Space_Type is abstract new ROM_Space_Type with record
      RAM_Handler : RAM_Space_Access;
   end record;

   procedure Initialize
     (Space       : out MBC_ROM_Space_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access;
      RAM_Handler : RAM_Space_Access);

   procedure Switch_Banks
     (Space            : in out MBC_ROM_Space_Type;
      Addressable_Bank : Addressable_Bank_Range;
      ROM_Bank         : ROM_Bank_Range);

   procedure ROM_Write
     (Space   : in out MBC_ROM_Space_Type'Class;
      Address : External_ROM_IO_Address;
      Content : Byte);
   pragma Inline (ROM_Write);

   subtype RAM_Enable_Address is Word range 16#0000# .. 16#1FFF#;

   procedure Enable_RAM
     (MBC     : in out MBC_ROM_Space_Type;
      Address : RAM_Enable_Address;
      Value   : Byte) is null;

   subtype Bank_Select_Address is Word range 16#2000# .. 16#5FFF#;

   procedure Select_Bank
     (MBC     : in out MBC_ROM_Space_Type;
      Address : Bank_Select_Address;
      Value   : Byte) is null;

   subtype Special_Address is Word range 16#6000# .. 16#7FFF#;

   procedure Write_Special
     (MBC     : in out MBC_ROM_Space_Type;
      Value   : Byte) is null;

end Gade.Cart.ROM_Space.MBC;
