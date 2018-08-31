package Gade.Cart.ROM is

   ROM_Bank_Size : constant := 16 * 1024;
   Max_ROM_Banks : constant := 512; -- 8 MB for MBC5 (512 x 8kByte)
   Max_ROM_Bytes : constant := ROM_Bank_Size * ROM_Bank_Size;

   type ROM_Bank_Count is range 0 .. Max_ROM_Banks;
   type ROM_Byte_Count is range 0 .. Max_ROM_Bytes;

   subtype ROM_Bank_Range is ROM_Bank_Count range 0 .. Max_ROM_Banks - 1;
   subtype ROM_Address_Range is ROM_Byte_Count range 0 .. Max_ROM_Bytes - 1;

   type ROM_Content_Type is array (ROM_Address_Range range <>) of aliased Byte;
   type ROM_Content_Access is access ROM_Content_Type;

   function Load (Path : String) return ROM_Content_Access;

   function Header (ROM : ROM_Content_Access) return Cart_Header_Access;

end Gade.Cart.ROM;
