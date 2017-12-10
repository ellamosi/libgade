package Gade.Cart.Banks.RAM.Blank is

   type Blank_RAM_Bank_Type is new RAM_Bank_Type with private;

   type Blank_RAM_Bank_Access is access all Blank_RAM_Bank_Type;

   function Singleton return Blank_RAM_Bank_Access;

   overriding
   procedure Read
     (Handler : Blank_RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : out Byte);

   overriding
   procedure Write
     (Handler : in out Blank_RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : Byte) is null;

private

   type Blank_RAM_Bank_Type is new RAM_Bank_Type with null record;

end Gade.Cart.Banks.RAM.Blank;
