generic
package Gade.Carts.Banks.Blank is

   type Blank_Bank is new Bank with private;

   type Blank_Bank_Access is access all Blank_Bank;

   subtype Blank_Bank_NN_Access is not null Blank_Bank_Access;

   overriding
   procedure Read
     (B       : in out Blank_Bank;
      Address : Bank_Address;
      V       : out Byte);

   function Singleton return Blank_Bank_NN_Access;

private

   type Blank_Bank is new Bank with null record;

end Gade.Carts.Banks.Blank;
