with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

package body Gade.Carts.Banks.Pools.Constructors is

   procedure Initialize
     (Pool  : out Bank_Pool;
      Banks : Bank_Array)
   is
      NN_Bank_Count : Bank_Count;
      Reference_Idx : Bank_Index;
   begin
      for I in Bank_Index'Range loop
         if Banks (I) /= null then
            NN_Bank_Count := Bank_Count (I) + 1;
            Pool.Banks (I) := Banks (I);
         else
            --  Loop the banks around to emulate unconnected address pins
            --  for smaller memory sizes
            Reference_Idx := Bank_Index (Bank_Count (I) mod NN_Bank_Count);
            Pool.Banks (I) := Banks (Reference_Idx);
         end if;
      end loop;
   end Initialize;

end Gade.Carts.Banks.Pools.Constructors;
