private with Gade.Carts.Banks;
private with Gade.Carts.Banks.RAM;
private with Gade.Carts.Bank_Pools;
private with Gade.Carts.Bank_Pools.RAM;

generic
   type Base_Cart is abstract new Cart with private;
   type Bank_Index is range <>;
package Gade.Carts.Mixins.Banked_RAM is

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

   Bank_Size : constant := 16#2000#;
   Bank_Address_Mask : constant Word := 16#3FFF#;

   package RAM_Space_Banks is new Gade.Carts.Banks (Bank_Size);
   package RAM_Banks is new RAM_Space_Banks.RAM;
   use RAM_Space_Banks, RAM_Banks;

   package RAM_Space_Bank_Pools is new Gade.Carts.Bank_Pools
     (Bank_Index     => Bank_Index,
      Bank_Type      => RAM_Bank,
      Bank_Access    => RAM_Bank_Access,
      Bank_NN_Access => RAM_Bank_NN_Access);
   package RAM_Bank_Pools is new RAM_Space_Bank_Pools.RAM;
   use RAM_Bank_Pools;

   type Banked_RAM_Cart is abstract new Base_Cart with record
      Accessible_Bank  : RAM_Bank_Access;
      Banks            : RAM_Bank_Pool;
      --  Should potentially by a bank concern: ? Yes.
      RAM_Address_Mask : Word;
   end record;

   function Rebase (Address : External_RAM_IO_Address;
                    Mask    : Word) return Bank_Address;
   pragma Inline (Rebase);

end Gade.Carts.Mixins.Banked_RAM;
