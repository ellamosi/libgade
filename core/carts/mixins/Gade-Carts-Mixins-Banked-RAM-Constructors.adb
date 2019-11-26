package body Gade.Carts.Mixins.Banked.RAM.Constructors is

   function Create_Offset_Bank
     (Content : RAM_Content_Access;
      Offset  : RAM_Address)
      return Bank_NN_Access
   is
      use RAM_Bank_Constructors;
   begin
      return Bank_Access (Create (Content, Offset));
   end Create_Offset_Bank;

   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access)
   is
      use RAM_Bank_Factories;

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
