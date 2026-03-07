with Gade.Carts.Mem.ROM; use Gade.Carts.Mem.ROM;
with Gade.Carts.Mem.RAM; use Gade.Carts.Mem.RAM;
with Gade.Carts.Mixins.ROM_RAM.Constructors;

generic
package Gade.Carts.Mixins.MBC.Constructors is

   package ROM_RAM_Constructors is new ROM_RAM_Mixin.Constructors;
   package ROM_Constructors renames ROM_RAM_Constructors.ROM_Constructors;
   package RAM_Constructors renames ROM_RAM_Constructors.RAM_Constructors;

   package RAM_Bank_Pool_Constructors
   renames ROM_RAM_Constructors.RAM_Bank_Pool_Constructors;
   use RAM_Bank_Pool_Constructors;

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
