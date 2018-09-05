package body Gade.Cart.Banked.RAM.MBC2 is

   procedure Initialize
     (Handler : out Handler_Type;
      Path    : String)
   is
   begin
      Handler.RAM := new Content;
      Load (Path, Handler.RAM.all);
      Handler.Path := new String'(Path);
   end Initialize;

   overriding procedure Read
     (Handler : Handler_Type;
      Address : Bank_Address;
      Value   : out Byte)
   is
   begin
      if Address < Max_Bytes then
         Value := Handler.RAM (Cart.RAM.Address (Address)) or Byte_Mask;
      else
         Value := Blank_Value;
      end if;
   end Read;

   overriding procedure Write
     (Handler : in out Handler_Type;
      Address : Bank_Address;
      Value   : Byte)
   is
   begin
      if Address < Max_Bytes then
         Handler.RAM (Cart.RAM.Address (Address)) := Value or Byte_Mask;
      end if;
   end Write;

   procedure Save (Handler : Handler_Type) is
   begin
      Save (Handler.Path.all, Handler.RAM.all);
   end Save;

end Gade.Cart.Banked.RAM.MBC2;
