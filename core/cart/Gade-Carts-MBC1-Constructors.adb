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
   begin
      Banked_ROM_Constructors.Initialize (C, Content);
      Banked_RAM_Constructors.Initialize (C, Header.RAM_Size, RAM_Path);
      --  TODO: Revise how banks are selected upon initialization/reset
      C.Low_Bank_Select := 1;
      C.High_Bank_Select := 0;
      C.Banking_Mode := ROM;
   end Initialize;

end Gade.Carts.MBC1.Constructors;
