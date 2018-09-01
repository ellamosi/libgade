package body Gade.Cart.Banked.RAM.Blank is

   Blank_Bank : aliased Blank_RAM_Bank_Type := (null record);
   Blank_Bank_Access : constant Blank_RAM_Bank_Access := Blank_Bank'Access;

   function Singleton return Blank_RAM_Bank_Access is
   begin
      return Blank_Bank_Access;
   end Singleton;

   overriding
   procedure Read
     (Handler : Blank_RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : out Byte)
   is
      pragma Unreferenced (Handler, Address);
   begin
      Value := Blank_Value;
   end Read;

end Gade.Cart.Banked.RAM.Blank;

