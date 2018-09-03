package Gade.Cart.RAM is

   Bank_Size : constant := 8 * 1024; -- Bytes
   Max_Banks : constant := 16; -- 128 kB for MBC5 (16 x 8kByte)
   Max_Bytes : constant := Max_Banks * Bank_Size;

   subtype Bank_Count_Type is Native_Unsigned range 0 .. Max_Banks;
   subtype Byte_Count_Type is Native_Unsigned range 0 .. Max_Bytes;

   subtype Bank_Index_Type is Bank_Count_Type range 0 .. Max_Banks - 1;
   subtype Address_Type is Byte_Count_Type range 0 .. Max_Bytes - 1;

   type Content_Type is array (Address_Type range <>) of Byte;
   type Content_Access is access Content_Type;

   procedure Load
     (Path    : String;
      Content : out Content_Type);

   procedure Save
     (Path    : String;
      Content : Content_Type);

end Gade.Cart.RAM;

