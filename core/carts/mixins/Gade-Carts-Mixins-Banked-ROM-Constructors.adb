package body Gade.Carts.Mixins.Banked.ROM.Constructors is

   procedure Initialize
     (C       : in out Banked_ROM_Cart'Class;
      Content : ROM_Content_NN_Access)
   is
      use ROM_Bank_Factories;

      BF : Default_Bank_Factory;
   begin
      Initialize (BF, Content);
      --  C.Content := Content;
      Initialize (C.Banks, BF);
      Reset (C);
   end Initialize;

   function Create_Offset_Bank
     (Content : ROM_Content_Access;
      Offset  : ROM_Address)
      return Bank_NN_Access
   is
      use ROM_Bank_Constructors;
   begin
      return Bank_Access (Create (Content, Offset));
   end Create_Offset_Bank;

end Gade.Carts.Mixins.Banked.ROM.Constructors;
