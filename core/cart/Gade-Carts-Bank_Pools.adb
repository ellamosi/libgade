package body Gade.Carts.Bank_Pools is

   function Select_Bank
     (Pool : Bank_Pool;
      I    : Bank_Index) return Bank_NN_Access
   is
   begin
      return Bank_NN_Access (Pool.Banks (I));
   end Select_Bank;

end Gade.Carts.Bank_Pools;
