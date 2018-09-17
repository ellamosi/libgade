package Gade.Cart.Banked.RAM is

   subtype Bank_Address is Word range 16#0000# .. 16#1FFF#;

   Address_Mask : constant Word := 16#1FFF#;

   type Handler_Type is abstract tagged private;
   type Handler_Access is access all Handler_Type'Class;

   procedure Read
     (Handler : in out Handler_Type;
      Address : Bank_Address;
      Value   : out Byte) is abstract;

   procedure Write
     (Handler : in out Handler_Type;
      Address : Bank_Address;
      Value   : Byte) is abstract;

private

   type Handler_Type is abstract tagged null record;

   type Path_Access is access constant String;

end Gade.Cart.Banked.RAM;
