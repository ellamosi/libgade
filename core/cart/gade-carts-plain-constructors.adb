with Gade.Carts.Banks.ROM.Constructors;
with Gade.Carts.Banks.RAM.Constructors;
with Gade.Carts.Banks.Blank;

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

   package ROM_Bank_Constructors is new ROM_Banks.Constructors;
   package RAM_Bank_Constructors is new RAM_Banks.Constructors;
   use ROM_Bank_Constructors, RAM_Bank_Constructors;

   package Blank_RAM_Banks is new RAM_Space_Banks.Blank;

   procedure Initialize
     (C        : out Plain_Cart'Class;
      Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String)
   is
      RAM_Size : constant Plain_RAM_Size_Type := Header.RAM_Size;
      RAM_Contents : RAM_Content_Access;
   begin
      Initialize (C.ROM, Content);
      if RAM_Size /= None then
         RAM_Contents := Create (RAM_Size);
         C.RAM := RAM_Space_Banks.Bank_Access (Create (RAM_Contents));
      else
         C.RAM := RAM_Space_Banks.Bank_Access (Blank_RAM_Banks.Singleton);
      end if;
      C.RAM_Path := new String'(RAM_Path);
   end Initialize;

end Gade.Carts.Plain.Constructors;
