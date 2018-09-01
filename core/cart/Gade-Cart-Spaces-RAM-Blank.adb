with Gade.Cart.Banked.RAM.Blank;

package body Gade.Cart.Spaces.RAM.Blank is

   Blank_Space : aliased Blank_RAM_Space_Type := (null record);
   Blank_Space_Access : constant Blank_RAM_Space_Access := Blank_Space'Access;

   function Singleton return Blank_RAM_Space_Access is
   begin
      return Blank_Space_Access;
   end Singleton;

   overriding procedure Read
     (Space   : in out Blank_RAM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
      pragma Unreferenced (Space, GB);
      Bank_Address : constant RAM_Bank_Address := To_Bank_Address (Address);
   begin
      Gade.Cart.Banked.RAM.Blank.Singleton.Read (Bank_Address, Content);
   end Read;

   overriding procedure Write
     (Space   : in out Blank_RAM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte)
   is
      pragma Unreferenced (Space, GB);
      Bank_Address : constant RAM_Bank_Address := To_Bank_Address (Address);
   begin
      Gade.Cart.Banked.RAM.Blank.Singleton.Write (Bank_Address, Content);
   end Write;

end Gade.Cart.Spaces.RAM.Blank;
