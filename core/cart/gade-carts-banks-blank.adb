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

   Singleton_Instance : Blank_Bank_Access;

   function Singleton return Blank_Bank_NN_Access is
   begin
      --  FIXME: Could make thread safe
      if Singleton_Instance = null then
         Singleton_Instance := new Blank_Bank;
      end if;
      return Singleton_Instance;
   end Singleton;

end Gade.Carts.Banks.Blank;
