with Gade.Carts.Mixins.ROM_RAM;

generic
   type Base_Cart is abstract new Cart with private;
   ROM_Banks, RAM_Banks : in Positive;
package Gade.Carts.Mixins.MBC is

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
   --  MBC2: 0000-1FFF - RAM Enable (Write Only) (Value based - Address restrictions)
   --  MBC3: 0000-1FFF - RAM and Timer Enable (Write Only) (Value based, always affects timer)
   --  MBC5: 0000-1FFF - RAM Enable (Write Only) (Value based)

   --  ROM Bank Select

   --  MBC1: 2000-3FFF - ROM Bank Number (Write Only) 0, 20h, 40h, 60h are mapped to +1
   --        4000-5FFF - RAM Bank Number - or - Upper Bits of ROM Bank Number (Write Only)
   --  MBC2: 0000-3FFF - ROM Bank Number (Write Only) - Address restrictions!
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

   subtype RAM_Enable_Address  is Word range 16#0000# .. 16#1FFF#;
   subtype Bank_Select_Address is Word range 16#2000# .. 16#5FFF#;
   subtype Special_Address     is Word range 16#6000# .. 16#7FFF#;

   package ROM_RAM_Mixin is new Gade.Carts.Mixins.ROM_RAM
     (Base_Cart => Base_Cart,
      ROM_Banks => ROM_Banks,
      RAM_Banks => RAM_Banks);
   use ROM_RAM_Mixin;

   type MBC_Cart is abstract new ROM_RAM_Cart with private;

   overriding
   procedure Write_ROM
     (C       : in out MBC_Cart;
      Address : External_ROM_IO_Address;
      V       : Byte);

   procedure Enable_RAM
     (C : in out MBC_Cart;
      V : Byte);

   procedure Select_Bank
     (C       : in out MBC_Cart;
      Address : Bank_Select_Address;
      Value   : Byte) is null;

   procedure Write_Special
     (C     : in out MBC_Cart;
      Value : Byte) is null;

private

   RAM_Enable_Mask  : constant := 16#0F#;
   RAM_Enable_Value : constant := 16#0A#;

   type MBC_Cart is abstract new ROM_RAM_Cart with null record;

end Gade.Carts.Mixins.MBC;
