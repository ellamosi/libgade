package body Gade.Carts.Plain.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String) return Plain_Cart_NN_Access
   is
      Result : constant Plain_Cart_NN_Access := new Plain_Cart;
   begin
      Initialize (Result.all, Content, Header, RAM_Path);
      return Result;
   end Create;

   procedure Initialize
     (C        : out Plain_Cart'Class;
      Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String)
   is
      RAM_Size : constant Plain_RAM_Size_Type := Header.RAM_Size;
   begin
      Plain_ROM_Constructors.Initialize (C, Content);
      Plain_RAM_Constructors.Initialize (C, RAM_Size, RAM_Path);
   end Initialize;

end Gade.Carts.Plain.Constructors;
