generic
package Gade.Carts.Mixins.Banked_ROM.Constructors is

   procedure Initialize
     (C       : in out Banked_ROM_Cart'Class;
      Content : ROM_Content_Access);

private

   procedure Initialize_Banks
     (Banks   : out ROM_Bank_Set;
      Content : ROM_Content_Access);

   --  Might end up belonging to reset
   procedure Initialize_Current_Banks
     (Current_Banks : out Current_ROM_Bank_Set;
      Banks         : ROM_Bank_Set);

end Gade.Carts.Mixins.Banked_ROM.Constructors;
