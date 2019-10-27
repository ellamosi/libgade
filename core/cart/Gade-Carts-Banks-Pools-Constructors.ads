generic
package Gade.Carts.Banks.Pools.Constructors is

   type Bank_Factory is abstract tagged null record;

   function Create_Bank
     (F : in out Bank_Factory;
      I : Bank_Index) return Bank_NN_Access is abstract;

   procedure Initialize
     (Pool  : out Bank_Pool;
      Banks : Bank_Array);

   procedure Initialize
     (Pool : out Bank_Pool;
      BF   : in out Bank_Factory'Class);

end Gade.Carts.Banks.Pools.Constructors;
