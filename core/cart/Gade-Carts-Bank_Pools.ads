generic
   type Bank_Index is range <>;
   type Bank_Type is limited private;
   type Bank_Access is access all Bank_Type;
   type Bank_NN_Access is not null access all Bank_Type;
package Gade.Carts.Bank_Pools is

   type Bank_Pool is abstract tagged private;

   function Select_Bank
     (Pool : Bank_Pool;
      I    : Bank_Index) return Bank_NN_Access;

private

   type NN_Bank_Array is array (Bank_Index) of Bank_NN_Access;

   type Bank_Array is array (Bank_Index) of Bank_Access;

   type Bank_Pool is tagged record
      Banks : Bank_Array;
   end record;

end Gade.Carts.Bank_Pools;
