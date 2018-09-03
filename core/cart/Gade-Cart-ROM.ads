package Gade.Cart.ROM is

   Bank_Size : constant := 16 * 1024; -- Bytes
   Max_Banks : constant := 512; -- 8 MB for MBC5 (512 x 16 kByte)
   Max_Bytes : constant := Max_Banks * Bank_Size;

   subtype Bank_Count_Type is Native_Unsigned range 0 .. Max_Banks;
   subtype Byte_Count_Type is Native_Unsigned range 0 .. Max_Bytes;

   subtype Bank_Index_Type is Bank_Count_Type range 0 .. Max_Banks - 1;
   subtype Address_Type is Byte_Count_Type range 0 .. Max_Bytes - 1;

   type Content_Type is array (Address_Type range <>) of aliased Byte;
   type Content_Access is access Content_Type;

   function Load (Path : String) return Content_Access;

end Gade.Cart.ROM;

