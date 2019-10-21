package body Gade.Carts.Banks.RAM.Constructors is

   function Create
     (Content : RAM_Content_NN_Access;
      Offset  : Memory_Content_Offset) return RAM_Bank_NN_Access
   is
      Result : constant RAM_Bank_NN_Access := new RAM_Bank;
   begin
      Initialize (Result.all, Content, Offset);
      return Result;
   end Create;

   procedure Initialize
     (B       : out RAM_Bank'Class;
      Content : RAM_Content_NN_Access;
      Offset  : Memory_Content_Offset)
   is
   begin
      RAM_Memory_Banks.Initialize_Full (B, Content, Offset);
   end Initialize;

   function Create
     (Content : RAM_Content_NN_Access) return Partial_RAM_Bank_NN_Access
   is
      Result : constant Partial_RAM_Bank_NN_Access := new Partial_RAM_Bank;
   begin
      Initialize (Result.all, Content);
      return Result;
   end Create;

   procedure Initialize
     (B       : out Partial_RAM_Bank'Class;
      Content : RAM_Content_NN_Access)
   is
   begin
      RAM_Memory_Banks.Initialize_Partial (B, Content);
   end Initialize;

end Gade.Carts.Banks.RAM.Constructors;
