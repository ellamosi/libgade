package body Gade.Carts.Banks.Blank is

   overriding
   procedure Read
     (B       : in out Blank_Bank;
      Address : Bank_Address;
      V       : out Byte)
   is
      pragma Unreferenced (B, Address);
   begin
      V := Blank_Value;
   end Read;

   Singleton_Instance : constant Blank_Bank_NN_Access := new Blank_Bank;

   function Singleton return Blank_Bank_NN_Access is
   begin
      return Singleton_Instance;
   end Singleton;

end Gade.Carts.Banks.Blank;
