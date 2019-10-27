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
      use Plain_RAM_Mixin.Banked_RAM_Spaces;

      RAM_Content : RAM_Content_Access := null;
   begin
      --  TODO: This conditional is ugly
      if Header.RAM_Size /= None then
         RAM_Content := Create (Header.RAM_Size, Max_Content_Size);
      end if;
      Plain_ROM_Constructors.Initialize (C, Content);
      Plain_RAM_Constructors.Initialize (C, RAM_Content, RAM_Path);
   end Initialize;

end Gade.Carts.Plain.Constructors;
