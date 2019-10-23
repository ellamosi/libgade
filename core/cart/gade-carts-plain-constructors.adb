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
      RAM_Contents : RAM_Content_Access;
   begin
      Initialize (C.ROM, Content, 0);
      if RAM_Size /= None then
         RAM_Contents := Create (RAM_Size);
         C.RAM := RAM_Space_Banks.Bank_Access (Create (RAM_Contents, 0));
      else
         C.RAM := RAM_Space_Banks.Bank_Access (Blank_RAM_Banks.Singleton);
      end if;
      C.RAM_Path := new String'(RAM_Path);
   end Initialize;

end Gade.Carts.Plain.Constructors;
