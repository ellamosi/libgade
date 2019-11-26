with Gade.Carts.Mixins.ROM_RAM;

generic
   type Base_Cart is abstract new Cart with private;
   ROM_Banks, RAM_Banks : in Positive;
package Gade.Carts.Mixins.MBC is

   --  All MBC style controllers implement similar behavior when writing to the
   --  ROM address space:
   --
   --  Writing to 0000-1FFF: Enables/Disables RAM
   --    MBC1: No special considerations
   --    MBC2: Address restrictions apply
   --    MBC3: No special considerations
   --    MBC5: No special considerations
   --
   --  Writing to 2000-5FFF: Perform Bank Select
   --    MBC1: 2000-3FFF - ROM Bank Number
   --          4000-5FFF - RAM Bank Number - or - Upper Bits of ROM Bank Number
   --    MBC2: 2000-3FFF - ROM Bank Number - Address restrictions!
   --          4000-5FFF - Unused
   --    MBC3: 2000-3FFF - ROM Bank Number
   --          4000-5FFF - RAM Bank Number - or - RTC Register Select
   --    MBC5: 2000-2FFF - Low 8 bits of ROM Bank Number
   --          3000-3FFF - High bit of ROM Bank Number
   --
   --  Writing to 6000-7FFF: Special Functions
   --    MBC1: Banking Mode Select
   --    MBC2: Unused
   --    MBC3: Latch RTC Data
   --    MBC5: Unused
   --
   --  So, aside from MBC2, which can override Write_ROM to handle its own
   --  address ranges and restrictions, it is possible to deffer a ROM write
   --  to a purpose specific handling procedure and have this behavior be
   --  shared across MBC1, MBC3 and MBC5.
   --
   --  As the logic to handle Bank Selection and the Special Functions change
   --  for each specific cart types, these will have to be defined in
   --  the concrete implementations.
   --
   --  The logic to check whether is a RAM Enable command seems to be the same
   --  on every MBC controller, and uses the lower 4 bits of the value being
   --  written to the ROM:
   --    16#A#                   - Enables RAM
   --    Values other than 16#A# - Disable RAM
   --
   --  MBC2 also shares this behavior as long as the address written to
   --  satisfies its specific ranges and restrictions.
   --
   --  As all MBC based carts support ROM and RAM banking, the mixin includes
   --  the instantiation of the ROM_RAM mixin, so it doesn't need to be
   --  re-instantiated for each of the individual MBC cart types.

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

   RAM_Enable_Mask  : constant Byte := 16#0F#;
   RAM_Enable_Value : constant Byte := 16#0A#;

   type MBC_Cart is abstract new ROM_RAM_Cart with null record;

end Gade.Carts.Mixins.MBC;
