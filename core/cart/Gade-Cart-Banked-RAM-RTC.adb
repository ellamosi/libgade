package body Gade.Cart.Banked.RAM.RTC is

   procedure Initialize
     (Handler : out Handler_Type;
      Path    : String)
   is
   begin
      Load (Path, Handler.Clk);
      Handler.Path := new String'(Path);
   end Initialize;

   overriding
   procedure Read
     (Handler : in out Handler_Type;
      Address : Bank_Address;
      Value   : out Byte)
   is
      pragma Unreferenced (Address);
   begin
      Read (Handler.Clk, Handler.Current, Value);
   end Read;

   procedure Set_Register
     (Handler : in out Handler_Type;
      Reg     : Register)
   is
   begin
      Handler.Current := Reg;
   end Set_Register;

   procedure Save (Handler : Handler_Type) is
   begin
      Save (Handler.Path.all, Handler.Clk);
   end Save;

end Gade.Cart.Banked.RAM.RTC;
