package body Gade.Carts.Banks.Pools is

   function Select_Bank
     (Pool : Bank_Pool;
      I    : Bank_Index) return Bank_NN_Access
   is
   begin
      return Bank_NN_Access (Pool.Banks (I));
   end Select_Bank;

end Gade.Carts.Banks.Pools;
