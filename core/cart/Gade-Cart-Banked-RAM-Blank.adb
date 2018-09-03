package body Gade.Cart.Banked.RAM.Blank is

   Blank_Handler : aliased Handler_Type := (null record);
   Blank_Handler_Access : constant Handler_Access := Blank_Handler'Access;

   function Singleton return Handler_Access is
   begin
      return Blank_Handler_Access;
   end Singleton;

   overriding
   procedure Read
     (Handler : Handler_Type;
      Address : Bank_Address;
      Value   : out Byte)
   is
      pragma Unreferenced (Handler, Address);
   begin
      Value := Blank_Value;
   end Read;

end Gade.Cart.Banked.RAM.Blank;
