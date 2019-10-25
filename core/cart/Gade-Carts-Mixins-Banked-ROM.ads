private with Gade.Carts.Banks.ROM;

generic
   type Base_Cart is abstract new Cart with private;
   Banks            : in Bank_Count;
   Accessible_Banks : in Bank_Count := 2;
package Gade.Carts.Mixins.Banked.ROM is

   type ROM_Bank_Index is
     new Bank_Index_Type range 0 .. Banks - 1;
   type Accessible_ROM_Bank_Index is
     new Bank_Index_Type range 0 .. Accessible_Banks - 1;

   type Banked_ROM_Cart is abstract new Base_Cart with private;

   overriding
   procedure Read_ROM
     (C       : in out Banked_ROM_Cart;
      Address : External_ROM_IO_Address;
      V       : out Byte);

   procedure Select_ROM_Bank
     (C  : in out Banked_ROM_Cart;
      AI : Accessible_ROM_Bank_Index;
      I  : ROM_Bank_Index);

private

   ROM_Space_Size : constant := 16#8000#;

   package Banked_ROM_Spaces is new Banked_Spaces
     (Base_Cart             => Base_Cart,
      Accessible_Bank_Index => Accessible_ROM_Bank_Index,
      Bank_Index            => ROM_Bank_Index,
      Address_Space         => External_ROM_IO_Address);
   use Banked_ROM_Spaces;

   package ROM_Banks is new Address_Space_Banks.ROM;
   use ROM_Banks, Address_Space_Banks;

   type Accessible_Bank_Array is
     array (Accessible_ROM_Bank_Index) of ROM_Bank_Access;

   type Banked_ROM_Cart is abstract new Banked_Space_Cart with record
      Accessible_Banks : Accessible_Bank_Array;
   end record;

   procedure Decode
     (Address   : External_ROM_IO_Address;
      Bank_Idx  : out Accessible_ROM_Bank_Index;
      Bank_Addr : out Bank_Address);
   pragma Inline (Decode);

end Gade.Carts.Mixins.Banked.ROM;
