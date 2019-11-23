with Gade.Carts.Constructors;
with Gade.Carts.Mem.RAM; use Gade.Carts.Mem.RAM;

package body Gade.Carts.Plain.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      Header   : Cart_Header;
      RAM_Path : String) return Plain_Cart_NN_Access
   is
      Result : constant Plain_Cart_NN_Access := new Plain_Cart;
   begin
      Initialize (Result.all, Content, Header, RAM_Path);
      return Result;
   end Create;

   procedure Initialize
     (C           : out Plain_Cart'Class;
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header;
      RAM_Path    : String)
   is
      use ROM_RAM_Mixin.Banked_RAM_Mixin.Banked_RAM_Spaces;

      RAM_Content : RAM_Content_Access;
      Has_Battery : Boolean;
      Savable     : Boolean;
   begin
      RAM_Content := Create (Header.RAM_Size, Max_Content_Size);
      Has_Battery := Cart_Type_Info_For_Cart (Header.Cart_Type).Battery;
      Savable := RAM_Content /= null and Has_Battery;
      Gade.Carts.Constructors.Initialize (Cart (C), RAM_Path, Savable);
      ROM_RAM_Constructors.Initialize (C, ROM_Content, RAM_Content);
   end Initialize;

end Gade.Carts.Plain.Constructors;
