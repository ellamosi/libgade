private with Gade.Carts.Banks;
private with Gade.Carts.Bank_Pools;
private with Gade.Carts.Memory_Contents;

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
   use Gade.Carts.Memory_Contents;

   Bank_Size : constant := 16#2000#;
   Bank_Address_Mask : constant Word := 16#1FFF#;

   type Path_Access is access String;

   package RAM_Space_Banks is new Gade.Carts.Banks (Bank_Size);
   use RAM_Space_Banks;

   package RAM_Bank_Pools is new Gade.Carts.Bank_Pools
     (Bank_Index     => Bank_Index,
      Bank_Type      => Bank,
      Bank_Access    => Bank_Access,
      Bank_NN_Access => Bank_NN_Access);
   use RAM_Bank_Pools;

   type Banked_RAM_Cart is abstract new Base_Cart with record
      Accessible_Bank : Bank_Access;
      Banks           : Bank_Pool;
      Content         : RAM_Content_Access;
      Path            : Path_Access;
   end record;

   function Rebase (Address : External_RAM_IO_Address) return Bank_Address;
   pragma Inline (Rebase);

end Gade.Carts.Mixins.Banked_RAM;
