with Gade.Carts.Mixins.Banked.RAM.Factories;

generic
package Gade.Carts.Mixins.Banked.RAM.Constructors is

   package RAM_Bank_Factories is new Gade.Carts.Mixins.Banked.RAM.Factories;
   use RAM_Bank_Factories;
   use Bank_Pool_Constructors;

   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access);

   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access;
      BF      : in out Bank_Factory'Class);

end Gade.Carts.Mixins.Banked.RAM.Constructors;
