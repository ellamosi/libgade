with Gade.Carts.Constructors;

package body Gade.Carts.MBC2.Constructors is

   overriding
   function Create_Bank
     (F : in out MBC2_RAM_Bank_Factory;
      I : Bank_Index) return Bank_NN_Access
   is
      pragma Unreferenced (I);
   begin
      if F.Bank = null then
         F.Bank := MBC2_RAM_Bank_Constructors.Create (F.Content);
      end if;
      return Bank_NN_Access (F.Bank);
   end Create_Bank;

   function Create
     (ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access;
      RAM_Path    : String)
      return MBC2_Cart_NN_Access
   is
      Result : constant MBC2_Cart_NN_Access := new MBC2_Cart;
   begin
      Initialize (Result.all, ROM_Content, Header, RAM_Path);
      return Result;
   end Create;

   procedure Initialize
     (C           : out MBC2_Cart'Class;
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access;
      RAM_Path    : String)
   is
      RAM_Content : RAM_Content_Access := null;
      BF          : MBC2_RAM_Bank_Factory;
      Has_Battery : Boolean;
      Savable     : Boolean;
   begin
      RAM_Content := Create (RAM_Bytes);
      BF.Content := RAM_Content;
      Has_Battery := Cart_Type_Info_For_Cart (Header.Cart_Type).Battery;
      Savable := RAM_Content /= null and Has_Battery;
      Gade.Carts.Constructors.Initialize (Cart (C), RAM_Path, Savable);
      ROM_RAM_Constructors.Initialize (C, ROM_Content, RAM_Content, BF);
      Reset (C);
   end Initialize;

end Gade.Carts.MBC2.Constructors;
