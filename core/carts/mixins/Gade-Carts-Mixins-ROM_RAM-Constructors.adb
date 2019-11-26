package body Gade.Carts.Mixins.ROM_RAM.Constructors is

   procedure Initialize
     (C           : in out ROM_RAM_Cart'Class;
      ROM_Content : ROM_Content_NN_Access;
      RAM_Content : RAM_Content_Access)
   is
   begin
      ROM_Constructors.Initialize (C, ROM_Content);
      RAM_Constructors.Initialize (C, RAM_Content);
   end Initialize;

   procedure Initialize
     (C                : in out Banked_RAM_Cart'Class;
      ROM_Content      : ROM_Content_NN_Access;
      RAM_Content      : RAM_Content_Access;
      RAM_Bank_Factory : in out Bank_Factory'Class)
   is
   begin
      ROM_Constructors.Initialize (C, ROM_Content);
      RAM_Constructors.Initialize (C, RAM_Content, RAM_Bank_Factory);
   end Initialize;

end Gade.Carts.Mixins.ROM_RAM.Constructors;
