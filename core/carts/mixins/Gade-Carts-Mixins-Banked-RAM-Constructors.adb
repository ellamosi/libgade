package body Gade.Carts.Mixins.Banked.RAM.Constructors is

   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access)
   is
      BF : Default_Bank_Factory;
   begin
      Initialize (BF, Content);
      Initialize (C, Content, BF);
   end Initialize;

   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access;
      BF      : in out Bank_Factory'Class)
   is
   begin
      C.Content := Content;
      Initialize (C.Banks, BF);
      Reset (C);
   end Initialize;

end Gade.Carts.Mixins.Banked.RAM.Constructors;
