with Gade.Cart.Memory_Contents; use Gade.Cart.Memory_Contents;
private with Gade.Cart.Banks;
private with Gade.Cart.Banks.Mem;

generic
   type Base_Cart is abstract new Cart with private;
   type Bank_Index is range <>;
package Gade.Cart.C2.Mixins.Banked_ROM is

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

   package Banks is new Gade.Cart.Banks (16#4000#, Bank_Index);
   package ROM_Banks is new Banks.Mem (ROM_Content, ROM_Content_Access);

   type ROM_Bank_Access is access ROM_Banks.Memory_Bank'Class;

   type ROM_Bank_Set is array (Bank_Index) of ROM_Bank_Access;
   type Current_ROM_Bank_Set is array (ROM_Bank_Location) of ROM_Bank_Access;

   type Banked_ROM_Cart is abstract new Base_Cart with record
      Current_ROM_Banks : Current_ROM_Bank_Set;
      ROM_Banks         : ROM_Bank_Set;
   end record;

end Gade.Cart.C2.Mixins.Banked_ROM;
