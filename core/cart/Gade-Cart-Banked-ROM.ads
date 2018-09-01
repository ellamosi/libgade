with Gade.Cart.ROM; use Gade.Cart.ROM;

package Gade.Cart.Banked.ROM is

   subtype ROM_Bank_Address is Word range 16#0000# .. 16#3FFF#;

   Bank_Address_Mask : constant Word := 16#3FFF#;

   type ROM_Bank_Content_Type is array (ROM_Bank_Address) of Byte;

   type ROM_Bank_Access is access constant ROM_Bank_Content_Type;

   type Memory_ROM_Bank_Type is tagged private;

   type Memory_ROM_Bank_Access is access Memory_ROM_Bank_Type;

   procedure Initialize
     (Bank    : out Memory_ROM_Bank_Type;
      Content : ROM_Content_Access);

   procedure Read
     (Bank    : Memory_ROM_Bank_Type;
      Address : ROM_Bank_Address;
      Value   : out Byte);

   procedure Set_Bank
     (Bank  : in out Memory_ROM_Bank_Type;
      Index : ROM_Bank_Range);

private

   type Non_Constant_ROM_Bank_Access is access ROM_Bank_Content_Type;

   type Memory_ROM_Bank_Type is tagged record
      Content : ROM_Content_Access;
      Offset  : ROM_Address_Range;
   end record;

end Gade.Cart.Banked.ROM;

