with Gade.Cart.Banked.RAM.Blank;

package body Gade.Cart.Spaces.RAM.Blank is

   Blank_Space : aliased Handler_Type := (null record);
   Blank_Space_Access : constant Handler_Access := Blank_Space'Access;

   function Singleton return Handler_Access is
   begin
      return Blank_Space_Access;
   end Singleton;

   overriding procedure Read
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
      pragma Unreferenced (Handler, GB);
      Bank_Address : constant RAM_Bank_Address := To_Bank_Address (Address);
   begin
      Gade.Cart.Banked.RAM.Blank.Singleton.Read (Bank_Address, Content);
   end Read;

   overriding procedure Write
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte)
   is
      pragma Unreferenced (Handler, GB);
      Bank_Address : constant RAM_Bank_Address := To_Bank_Address (Address);
   begin
      Gade.Cart.Banked.RAM.Blank.Singleton.Write (Bank_Address, Content);
   end Write;

end Gade.Cart.Spaces.RAM.Blank;

