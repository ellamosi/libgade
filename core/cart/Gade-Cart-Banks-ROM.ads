with Ada.Sequential_IO;
with Ada.Unchecked_Conversion;

package Gade.Cart.Banks.ROM is

   --  Each bank is 16 kB
   subtype ROM_Bank_Address is Word range 16#0000# .. 16#3FFF#;

   Bank_Address_Mask : constant Word := 16#3FFF#;

   type ROM_Bank_Content_Type is array (ROM_Bank_Address) of Byte;

   type ROM_Bank_Access is access constant ROM_Bank_Content_Type;

   function Convert is new Ada.Unchecked_Conversion
     (Source => ROM_Bank_Access,
      Target => Cart_Header_Access);

   package ROM_Bank_IO is new Ada.Sequential_IO (ROM_Bank_Content_Type);

   procedure Load
     (File : ROM_Bank_IO.File_Type;
      Bank : out ROM_Bank_Access);

private

   type Non_Constant_ROM_Bank_Access is access ROM_Bank_Content_Type;

end Gade.Cart.Banks.ROM;
