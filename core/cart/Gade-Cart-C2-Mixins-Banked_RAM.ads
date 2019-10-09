with Gade.Cart.Memory_Contents; use Gade.Cart.Memory_Contents;
private with Gade.Cart.Banks;
private with Gade.Cart.Banks.Mem;

generic
   type Base_Cart is abstract new Cart with private;
   type Bank_Index is range <>;
package Gade.Cart.C2.Mixins.Banked_RAM is

   type Banked_RAM_Cart is abstract new Base_Cart with private;

   overriding
   procedure Read_RAM
     (C       : in out Banked_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte);

   overriding
   procedure Write_RAM
     (C       : in out Banked_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte);

   procedure Select_RAM_Bank
     (C : in out Banked_RAM_Cart;
      I : Bank_Index);

private

   Bank_Address_Mask : constant Word := 16#3FFF#;

   subtype Bank_Address is Word range 16#0000# .. 16#1FFF#;

   package Banks is new Gade.Cart.Banks (16#2000#, Bank_Index);
   package RAM_Banks is new Banks.Mem (RAM_Content, RAM_Content_Access);

   type RAM_Bank_Access is access RAM_Banks.Memory_Bank;
   type ROM_Bank_Set is array (Bank_Index) of RAM_Bank_Access;

   type Banked_RAM_Cart is abstract new Base_Cart with record
      Current_RAM_Bank : RAM_Bank_Access;
      RAM_Banks        : ROM_Bank_Set;
   end record;

end Gade.Cart.C2.Mixins.Banked_RAM;
