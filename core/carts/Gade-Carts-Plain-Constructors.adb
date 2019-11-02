with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Carts.Plain.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String) return Plain_Cart_NN_Access
   is
      Result : constant Plain_Cart_NN_Access := new Plain_Cart;
   begin
      Put_Line ("Initializing");
      Initialize (Result.all, Content, Header, RAM_Path);
      Put_Line ("Initialized");
      return Result;
   end Create;

   procedure Initialize
     (C        : out Plain_Cart'Class;
      Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String)
   is
      use Plain_RAM_Mixin.Banked_RAM_Spaces;

      RAM_Content : RAM_Content_Access;
   begin
      Put_Line ("Allocating RAM");
      RAM_Content := Create (Header.RAM_Size, Max_Content_Size);
      Put_Line ("Initializing ROM");
      Plain_ROM_Constructors.Initialize (C, Content);
      Put_Line ("Initializing RAM");
      Plain_RAM_Constructors.Initialize (C, RAM_Content, RAM_Path);
   end Initialize;

end Gade.Carts.Plain.Constructors;
