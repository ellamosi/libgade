generic
package Gade.Carts.Banks.ROM is

   type ROM_Bank is new Bank with private;
   type Partial_ROM_Bank is new Bank with private;

   type ROM_Bank_Access is access all ROM_Bank;
   type Partial_ROM_Bank_Access is access all Partial_ROM_Bank;

   subtype ROM_Bank_NN_Access is not null ROM_Bank_Access;
   subtype Partial_ROM_Bank_NN_Access is not null Partial_ROM_Bank_Access;

private

   package ROM_Memory_Banks is new Memory_Bank_Mixin
     (Base_Bank         => Bank,
      Content_Type      => ROM_Content,
      Content_NN_Access => ROM_Content_NN_Access);
   use ROM_Memory_Banks;

   type ROM_Bank is new Memory_Bank with null record;
   type Partial_ROM_Bank is new Partial_Memory_Bank with null record;

end Gade.Carts.Banks.ROM;
