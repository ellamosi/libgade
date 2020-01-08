with Ada.Text_IO;

package body Gade.Carts.Banks.Pools is

   function Select_Bank
     (Pool : Bank_Pool;
      I    : Bank_Index) return Bank_NN_Access
   is
   begin
      return Bank_NN_Access (Pool.Banks (I));
   end Select_Bank;

   procedure Finalize (Pool : in out Bank_Pool) is
   begin
      Ada.Text_IO.Put_Line ("Banks.Pools.Finalize");
      for B of Pool.Banks loop
         Free (B);
      end loop;
   end Finalize;

end Gade.Carts.Banks.Pools;
