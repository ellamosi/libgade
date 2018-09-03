package Gade.Cart.Banked.RAM.Blank is

   type Handler_Type is new RAM.Handler_Type with private;
   type Handler_Access is access all Handler_Type;

   function Singleton return Handler_Access;

   overriding
   procedure Read
     (Handler : Handler_Type;
      Address : Bank_Address;
      Value   : out Byte);

   overriding
   procedure Write
     (Handler : in out Handler_Type;
      Address : Bank_Address;
      Value   : Byte) is null;

private

   type Handler_Type is new RAM.Handler_Type with null record;

end Gade.Cart.Banked.RAM.Blank;
