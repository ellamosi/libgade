generic
package Gade.Carts.Banks.Pools.Constructors is

   --  TODO: Potentially use Gade.Carts.Banks.Factories for this
   --  Might help with Banked ROM/RAM needing to with the Pools, which should
   --  not really be their concern. Additionally allows having a Constructor
   --  child package for factories without having to rely on the awkward
   --  Constructors.Constructors or Constructors.Factory_Constructors naming.
   --  This would require a new generic instance, though.
   --  Maybe Gade.Carts.Banks.Pools.Factories instead as compromise?
   type Bank_Factory is abstract tagged null record;

   function Create_Bank
     (F : in out Bank_Factory;
      I : Bank_Index) return Bank_NN_Access is abstract;

   --  TODO Remove this one
   procedure Initialize
     (Pool  : out Bank_Pool;
      Banks : Bank_Array);

   procedure Initialize
     (Pool : out Bank_Pool;
      BF   : in out Bank_Factory'Class);

end Gade.Carts.Banks.Pools.Constructors;
