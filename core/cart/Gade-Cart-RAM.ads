package Gade.Cart.RAM is

   RAM_Bank_Size : constant := 8 * 1024;
   Max_RAM_Banks : constant := 16; --  128 kB for MBC5 (16 x 8kByte)
   Max_RAM_Bytes : constant := Max_RAM_Banks * RAM_Bank_Size;

   subtype RAM_Bank_Count_Type is Native_Unsigned range 0 .. Max_RAM_Banks;
   subtype RAM_Byte_Count_Type is Native_Unsigned range 0 .. Max_RAM_Bytes;

   subtype RAM_Bank_Range is RAM_Bank_Count_Type range 0 .. Max_RAM_Banks - 1;
   subtype RAM_Address_Range is RAM_Byte_Count_Type range 0 .. Max_RAM_Bytes - 1;

   type RAM_Content_Type is array (RAM_Address_Range range <>) of Byte;
   type RAM_Content_Access is access RAM_Content_Type;

   procedure Load
     (Path    : String;
      Content : out RAM_Content_Type);

   procedure Save
     (Path    : String;
      Content : RAM_Content_Type);

end Gade.Cart.RAM;

