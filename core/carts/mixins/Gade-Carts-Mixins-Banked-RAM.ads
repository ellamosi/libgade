with Gade.Carts.Banks.Blank;
with Gade.Carts.Banks.RAM;
with Gade.Carts.Banks.RTC;
with Gade.Carts.Banks.RAM.MBC2;

generic
   type Base_Cart is abstract new Cart with private;
   Banks              : in Bank_Count;
   Enabled_By_Default : in Boolean := False;
package Gade.Carts.Mixins.Banked.RAM is

   package Banked_RAM_Spaces is new Banked_Spaces
     (Banks            => Banks,
      Accessible_Banks => 1,
      Address_Space    => External_RAM_IO_Address);
   use Banked_RAM_Spaces;

   --  Make bank package instances visible so specialized factories do not
   --  require re-instancing them, resulting in incompatible types.
   package RAM_Banks is new Address_Space_Banks.RAM;
   package MBC2_RAM_Banks is new RAM_Banks.MBC2;
   package RTC_Banks is new Address_Space_Banks.RTC;
   package Blank_Banks is new Address_Space_Banks.Blank;

   type Banked_RAM_Cart is abstract new Base_Cart with private;

   procedure Reset_RAM (C : in out Banked_RAM_Cart);

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

   procedure Enable_RAM (C : in out Banked_RAM_Cart; Enable : Boolean);

   function Is_RAM_Enabled (C : Banked_RAM_Cart) return Boolean;

private
   use Address_Space_Banks, Bank_Pools;

   Enabled_Default : constant Boolean := Enabled_By_Default;

   package RAM_Space_Carts is new Banked_Space_Carts
     (Base_Cart => Base_Cart,
      BS        => Banked_RAM_Spaces);
   use RAM_Space_Carts;

   type Banked_RAM_Cart is abstract new Banked_Space_Cart with record
      Accessible_Bank  : Bank_Access;
      Accessible_Index : Bank_Index;
      Enabled          : Boolean;
      Content          : RAM_Content_Access;
   end record;

   overriding
   procedure Load_RAM_File
     (C    : in out Banked_RAM_Cart;
      File : Ada.Streams.Stream_IO.File_Type);

   overriding
   procedure Save_RAM_File
     (C    : in out Banked_RAM_Cart;
      File : Ada.Streams.Stream_IO.File_Type);

   function Decode (Address : External_RAM_IO_Address) return Bank_Address;
   pragma Inline (Decode);

end Gade.Carts.Mixins.Banked.RAM;
