with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

generic
package Gade.Carts.Mixins.Banked_ROM.Constructors is

   procedure Initialize
     (C       : in out Banked_ROM_Cart'Class;
      Content : ROM_Content_NN_Access);

private

   procedure Initialize_Banks
     (Pool    : out Bank_Pool;
      Content : ROM_Content_NN_Access);

   --  Might end up belonging to reset
   procedure Initialize_Accessible_Banks
     (Accessible_Banks : out Accessible_Bank_Array;
      Pool             : Bank_Pool);

end Gade.Carts.Mixins.Banked_ROM.Constructors;
