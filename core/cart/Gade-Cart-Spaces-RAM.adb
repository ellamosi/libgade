package body Gade.Cart.Spaces.RAM is

   overriding
   procedure Read
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
      pragma Unreferenced (GB);
      Bank_Addr : constant Bank_Address := Address and Address_Mask;
   begin
      Handler.Current_Bank.Read (Bank_Addr, Content);
   end Read;

   overriding
   procedure Write
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte)
   is
      pragma Unreferenced (GB);
      Bank_Addr : constant Bank_Address := Address and Address_Mask;
   begin
      Handler.Current_Bank.Write (Bank_Addr, Content);
   end Write;

   function To_Bank_Address (Address : Word) return Bank_Address is
   begin
      return Address and Address_Mask;
   end To_Bank_Address;

end Gade.Cart.Spaces.RAM;
