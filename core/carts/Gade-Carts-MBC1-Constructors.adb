with Gade.Carts.Constructors;
with Gade.Carts.Mem.RAM; use Gade.Carts.Mem.RAM;

package body Gade.Carts.MBC1.Constructors is

   function Create
     (ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access;
      RAM_Path    : String)
      return MBC1_Cart_NN_Access
   is
      Result : constant MBC1_Cart_NN_Access := new MBC1_Cart;
   begin
      Initialize (Result.all, ROM_Content, Header, RAM_Path);
      return Result;
   end Create;

   procedure Initialize
     (C           : out MBC1_Cart'Class;
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access;
      RAM_Path    : String)
   is
      use MBC_Mixin.ROM_RAM_Mixin.Banked_RAM_Mixin.Banked_RAM_Spaces;

      RAM_Content : RAM_Content_Access;
      Has_Battery : Boolean;
      Savable     : Boolean;
   begin
      RAM_Content := Create (Header.RAM_Size, Max_Content_Size);
      Has_Battery := Cart_Type_Info_For_Cart (Header.Cart_Type).Battery;
      Savable := RAM_Content /= null and Has_Battery;
      Gade.Carts.Constructors.Initialize (Cart (C), RAM_Path, Savable);
      MBC_Constructors.Initialize (C, ROM_Content, RAM_Content);
      C.Reset;
   end Initialize;

end Gade.Carts.MBC1.Constructors;
