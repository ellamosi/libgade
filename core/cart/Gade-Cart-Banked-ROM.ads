with Gade.Cart.ROM; use Gade.Cart.ROM;

package Gade.Cart.Banked.ROM is

   subtype Bank_Address is Word range 16#0000# .. 16#3FFF#;

   Address_Mask : constant Word := 16#3FFF#;

   type Handler_Type is tagged private;
   type Handler_Access is access Handler_Type;

   procedure Initialize
     (Handler : out Handler_Type;
      Content : Content_Access);

   procedure Read
     (Handler : Handler_Type;
      Addr    : Bank_Address;
      Value   : out Byte);

   procedure Set_Bank
     (Handler : in out Handler_Type;
      Index   : Bank_Index);

private

   type Handler_Type is tagged record
      ROM     : Content_Access;
      Offset  : Address;
   end record;

end Gade.Cart.Banked.ROM;
