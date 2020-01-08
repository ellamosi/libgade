generic
   type Bank_Index is range <>;
package Gade.Carts.Banks.Pools is

   type Bank_Array is array (Bank_Index) of Bank_Access;

   type Bank_Pool is private;

   function Select_Bank
     (Pool : Bank_Pool;
      I    : Bank_Index) return Bank_NN_Access;

   procedure Finalize (Pool : in out Bank_Pool);

private

   type Bank_Pool is record
      Banks         : Bank_Array;
      Created_Banks : Bank_Array;
   end record;

end Gade.Carts.Banks.Pools;
