with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

generic
package Gade.Carts.Bank_Pools.RAM is

   type RAM_Bank_Pool is new Bank_Pool with private;

   procedure Save (B : RAM_Bank_Pool) is null; --  TODO

private

   type RAM_Bank_Pool is new Bank_Pool with record
      Content : RAM_Content_Access;
      Path    : access String;
   end record;

end Gade.Carts.Bank_Pools.RAM;
