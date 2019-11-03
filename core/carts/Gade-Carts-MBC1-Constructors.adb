package body Gade.Carts.MBC1.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String)
      return MBC1_Cart_NN_Access
   is
      Result : constant MBC1_Cart_NN_Access := new MBC1_Cart;
   begin
      Initialize (Result.all, Content, Header, RAM_Path);
      return Result;
   end Create;

   procedure Initialize
     (C        : out MBC1_Cart'Class;
      Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String)
   is
      use Banked_RAM_Mixin.Banked_RAM_Spaces;

      RAM_Content : RAM_Content_Access := null;
   begin
      RAM_Content := Create (Header.RAM_Size, Max_Content_Size);
      Banked_ROM_Constructors.Initialize (C, Content);
      Banked_RAM_Constructors.Initialize (C, RAM_Content, RAM_Path);
      C.Reset;
   end Initialize;

end Gade.Carts.MBC1.Constructors;
