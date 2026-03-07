with Gade.Carts.Mem.ROM; use Gade.Carts.Mem.ROM;
with Gade.Carts.Mem.RAM; use Gade.Carts.Mem.RAM;
with Gade.Carts.Mixins.Banked.ROM.Constructors;
with Gade.Carts.Mixins.Banked.RAM.Constructors;

generic
package Gade.Carts.Mixins.ROM_RAM.Constructors is

   package ROM_Constructors is new Banked_ROM_Mixin.Constructors;
   package RAM_Constructors is new Banked_RAM_Mixin.Constructors;

   package RAM_Bank_Pool_Constructors
   renames RAM_Constructors.Bank_Pool_Constructors;
   use RAM_Bank_Pool_Constructors;

   procedure Initialize
     (C           : in out ROM_RAM_Cart'Class;
      ROM_Content : ROM_Content_NN_Access;
      RAM_Content : RAM_Content_Access);

   procedure Initialize
     (C                : in out Banked_RAM_Cart'Class;
      ROM_Content      : ROM_Content_NN_Access;
      RAM_Content      : RAM_Content_Access;
      RAM_Bank_Factory : in out Bank_Factory'Class);

end Gade.Carts.Mixins.ROM_RAM.Constructors;
