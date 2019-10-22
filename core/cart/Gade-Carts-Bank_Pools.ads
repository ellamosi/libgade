generic
   type Bank_Index is range <>;
   type Bank_Type is abstract tagged private;
   type Bank_Access is access all Bank_Type'Class;
   type Bank_NN_Access is not null access all Bank_Type'Class;
package Gade.Carts.Bank_Pools is

   type Bank_Array is array (Bank_Index) of Bank_Access;

   type Bank_Pool is private;

   function Select_Bank
     (Pool : Bank_Pool;
      I    : Bank_Index) return Bank_NN_Access;

private

   type Bank_Pool is record
      Banks : Bank_Array;
   end record;

end Gade.Carts.Bank_Pools;
