private with Gade.Carts.Banks.Blank;
private with Gade.Carts.Banks.RAM;

generic
   type Base_Cart is abstract new Cart with private;
   Banks              : in Bank_Count;
   Enabled_By_Default : in Boolean := False;
package Gade.Carts.Mixins.Banked.RAM is

   type RAM_Bank_Index is new Bank_Index_Type range 0 .. Banks - 1;

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
      I : RAM_Bank_Index);

   procedure Enable_RAM (C : in out Banked_RAM_Cart; Enable : Boolean);

private

   Enabled_Default : constant Boolean := Enabled_By_Default;

   type Path_Access is access String;

   package Banked_ROM_Spaces is new Banked_Spaces
     (Base_Cart             => Base_Cart,
      Accessible_Bank_Index => Unbanked_Bank_Index,
      Bank_Index            => RAM_Bank_Index,
      Address_Space         => External_RAM_IO_Address);
   use Banked_ROM_Spaces;

   package RAM_Banks is new Address_Space_Banks.RAM;
   package Blank_Banks is new Address_Space_Banks.Blank;
   use Address_Space_Banks, Bank_Pools;

   type Banked_RAM_Cart is abstract new Banked_Space_Cart with record
      Accessible_Bank  : Bank_Access;
      Accessible_Index : RAM_Bank_Index;
      Enabled          : Boolean;
      Content          : RAM_Content_Access;
      Path             : Path_Access;
   end record;

   function Decode (Address : External_RAM_IO_Address) return Bank_Address;
   pragma Inline (Decode);

end Gade.Carts.Mixins.Banked.RAM;
