with Gade.Carts.Mem.ROM; use Gade.Carts.Mem.ROM;
private with Gade.Carts.Banks.ROM;

generic
   type Base_Cart is abstract new Cart with private;
   Banks            : in Positive;
   Accessible_Banks : in Positive := 2;
package Gade.Carts.Mixins.Banked.ROM is

   package Banked_ROM_Spaces is new Banked_Spaces
     (Banks            => Banks,
      Accessible_Banks => Accessible_Banks,
      Address_Space    => External_ROM_IO_Address,
      Content_Size     => ROM_Content_Size);
   use Banked_ROM_Spaces;

   type Banked_ROM_Cart is abstract new Base_Cart with private;

   procedure Reset_ROM (C : in out Banked_ROM_Cart);

   overriding
   procedure Read_ROM
     (C       : in out Banked_ROM_Cart;
      Address : External_ROM_IO_Address;
      V       : out Byte);

   procedure Select_ROM_Bank
     (C  : in out Banked_ROM_Cart;
      AI : Accessible_Bank_Index;
      I  : Bank_Index);

private

   ROM_Space_Size : constant := 16#8000#;

   package ROM_Space_Carts is new Banked_Space_Carts
     (Base_Cart => Base_Cart,
      BS        => Banked_ROM_Spaces);
   use ROM_Space_Carts;

   package ROM_Banks is new Address_Space_Banks.ROM;
   use ROM_Banks, Address_Space_Banks;

   type Accessible_Bank_Array is
     array (Accessible_Bank_Index) of ROM_Bank_Access;

   type Banked_ROM_Cart is abstract new Banked_Space_Cart with record
      Accessible_Banks : Accessible_Bank_Array;
   end record;

   procedure Decode
     (Address   : External_ROM_IO_Address;
      Bank_Idx  : out Accessible_Bank_Index;
      Bank_Addr : out Bank_Address);
   pragma Inline (Decode);

end Gade.Carts.Mixins.Banked.ROM;
