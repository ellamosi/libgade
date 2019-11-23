with Gade.Carts.Mem.ROM; use Gade.Carts.Mem.ROM;
with Gade.Carts.Banks.ROM;
with Gade.Carts.Banks.Blank;

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

   --  Make bank package instances visible so specialized factories do not
   --  require re-instancing them, resulting in incompatible types.
   package Address_Space_Banks renames Banked_ROM_Spaces.Address_Space_Banks;
   package ROM_Banks is new Address_Space_Banks.ROM;
   package Blank_Banks is new Address_Space_Banks.Blank;

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
   use Address_Space_Banks, ROM_Banks;

   package ROM_Space_Carts is new Banked_Space_Carts
     (Base_Cart => Base_Cart,
      BS        => Banked_ROM_Spaces);
   use ROM_Space_Carts;

   type Accessible_Bank_Array is
     array (Accessible_Bank_Index) of ROM_Bank_Access;

   type Banked_ROM_Cart is abstract new Banked_Space_Cart with record
      Content          : ROM_Content_Access;
      Accessible_Banks : Accessible_Bank_Array;
   end record;

   procedure Decode
     (Address   : External_ROM_IO_Address;
      Bank_Idx  : out Accessible_Bank_Index;
      Bank_Addr : out Bank_Address);
   pragma Inline (Decode);

end Gade.Carts.Mixins.Banked.ROM;
