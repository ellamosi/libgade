private with Gade.Carts.Banks;
private with Gade.Carts.Banks.ROM;
private with Gade.Carts.Bank_Pools;
private with Gade.Carts.Bank_Pools.ROM;

generic
   type Base_Cart is abstract new Cart with private;
   type Bank_Index is range <>;
   --  Could parametrize the number of accessible banks
   --  (MBC6 uses 4 instead of the regular 2)
   --  If this is done, Plain ROM and Plain/Banked RAMs could share
   --  some of this implementation.
package Gade.Carts.Mixins.Banked_ROM is

   type ROM_Bank_Location is (Bank0, Bank1);

   type Banked_ROM_Cart is abstract new Base_Cart with private;

   overriding
   procedure Read_ROM
     (C       : in out Banked_ROM_Cart;
      Address : External_ROM_IO_Address;
      V       : out Byte);

   procedure Select_ROM_Bank
     (C : in out Banked_ROM_Cart;
      L : ROM_Bank_Location;
      I : Bank_Index);

private

   Bank_Size : constant := 16#4000#;
   Bank_Address_Mask : constant Word := 16#3FFF#;

   subtype Bank0_Address is External_ROM_IO_Address range 16#0000# .. 16#3FFF#;
   subtype Bank1_Address is External_ROM_IO_Address range 16#4000# .. 16#7FFF#;

   subtype Bank_Address is Word range 16#0000# .. 16#3FFF#;

   package ROM_Space_Banks is new Gade.Carts.Banks (Bank_Size);
   package ROM_Banks is new ROM_Space_Banks.ROM;
   use ROM_Banks;

   package ROM_Space_Bank_Pools is new Gade.Carts.Bank_Pools
     (Bank_Index     => Bank_Index,
      Bank_Type      => ROM_Bank,
      Bank_Access    => ROM_Bank_Access,
      Bank_NN_Access => ROM_Bank_NN_Access);
   package ROM_Bank_Pools is new ROM_Space_Bank_Pools.ROM;
   use ROM_Bank_Pools;

   type Current_ROM_Bank_Set is array (ROM_Bank_Location) of ROM_Bank_Access;

   type Banked_ROM_Cart is abstract new Base_Cart with record
      Accessible_Banks : Current_ROM_Bank_Set;
      Banks            : ROM_Bank_Pool;
   end record;

end Gade.Carts.Mixins.Banked_ROM;
