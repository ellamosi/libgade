with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;
private with Gade.Carts.Banks.ROM.Constructors;
private with Gade.Carts.Banks.Pools.Constructors;

generic
package Gade.Carts.Mixins.Banked_ROM.Constructors is

   procedure Initialize
     (C       : in out Banked_ROM_Cart'Class;
      Content : ROM_Content_NN_Access);

private

   package ROM_Bank_Constructors is new ROM_Banks.Constructors;
   package ROM_Bank_Pool_Constructors is new ROM_Bank_Pools.Constructors;

   procedure Initialize_Banks
     (Pool    : out Bank_Pool;
      Content : ROM_Content_NN_Access);

   --  Might end up belonging to reset
   procedure Initialize_Accessible_Banks
     (Accessible_Banks : out Accessible_Bank_Array;
      Pool             : Bank_Pool);

end Gade.Carts.Mixins.Banked_ROM.Constructors;
