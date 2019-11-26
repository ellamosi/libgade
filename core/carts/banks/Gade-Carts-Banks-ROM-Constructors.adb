package body Gade.Carts.Banks.ROM.Constructors is

   function Create
     (Content : ROM_Content_NN_Access;
      Offset  : ROM_Address) return ROM_Bank_NN_Access
   is
      Result : constant ROM_Bank_NN_Access := new ROM_Bank;
   begin
      Constructors.Initialize (Result.all, Content, Offset);
      return Result;
   end Create;

   procedure Initialize
     (B       : out ROM_Bank'Class;
      Content : ROM_Content_NN_Access;
      Offset  : ROM_Address)
   is
   begin
      ROM_Memory_Banks.Initialize (B, Content, Offset);
   end Initialize;

end Gade.Carts.Banks.ROM.Constructors;
