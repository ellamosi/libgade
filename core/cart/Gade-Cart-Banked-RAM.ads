package Gade.Cart.Banked.RAM is

   subtype RAM_Bank_Address is Word range 16#0000# .. 16#1FFF#;

   Bank_Address_Mask : constant Word := 16#1FFF#;

   type RAM_Bank_Type is abstract tagged private;

   type RAM_Bank_Access is access all RAM_Bank_Type'Class;

   procedure Read
     (Handler : RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : out Byte) is abstract;

   procedure Write
     (Handler : in out RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : Byte) is abstract;

private

   type RAM_Bank_Type is abstract tagged null record;

end Gade.Cart.Banked.RAM;
