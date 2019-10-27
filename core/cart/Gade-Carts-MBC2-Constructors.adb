package body Gade.Carts.MBC2.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String)
      return MBC2_Cart_NN_Access
   is
      Result : constant MBC2_Cart_NN_Access := new MBC2_Cart;
   begin
      Initialize (Result.all, Content, Header, RAM_Path);
      return Result;
   end Create;

   procedure Initialize
     (C        : out MBC2_Cart'Class;
      Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String)
   is

   begin
      Banked_ROM_Constructors.Initialize (C, Content);
      --  TODO: Got to adjust constructor so special RAM kind can be created
      --  with the right bank types.
      MBC2_RAM_Constructors.Initialize (C, Header.RAM_Size, RAM_Path);
   end Initialize;

end Gade.Carts.MBC2.Constructors;
