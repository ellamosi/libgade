with Gade.Carts.Mem.RAM; use Gade.Carts.Mem.RAM;

generic
package Gade.Carts.Banks.RAM is

   type RAM_Bank is new Bank with private;

   type RAM_Bank_Access is access all RAM_Bank;

   subtype RAM_Bank_NN_Access is not null RAM_Bank_Access;

   overriding
   procedure Write
     (B       : in out RAM_Bank;
      Address : Bank_Address;
      V       : Byte);

private

   package RAM_Memory_Banks is new Memory_Bank_Mixin
     (Base_Bank         => Bank,
      Address           => RAM_Address,
      Content           => RAM_Content,
      Content_Access    => RAM_Content_Access,
      Content_NN_Access => RAM_Content_NN_Access);
   use RAM_Memory_Banks;

   type RAM_Bank is new Memory_Bank with null record;

end Gade.Carts.Banks.RAM;
