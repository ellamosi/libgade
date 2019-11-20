with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;
with Gade.Carts.Mixins.ROM_RAM.Constructors;

generic
package Gade.Carts.Mixins.MBC.Constructors is

   package ROM_RAM_Constructors is new ROM_RAM_Mixin.Constructors;
   use ROM_RAM_Constructors;
   use RAM_Constructors.RAM_Bank_Factories.Bank_Pool_Constructors;

   procedure Initialize
     (C           : in out MBC_Cart'Class;
      ROM_Content : ROM_Content_NN_Access;
      RAM_Content : RAM_Content_Access);

   procedure Initialize
     (C                : in out MBC_Cart'Class;
      ROM_Content      : ROM_Content_NN_Access;
      RAM_Content      : RAM_Content_Access;
      RAM_Bank_Factory : in out Bank_Factory'Class);

end Gade.Carts.Mixins.MBC.Constructors;
