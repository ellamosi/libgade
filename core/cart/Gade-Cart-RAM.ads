package Gade.Cart.RAM is

   Bank_Size : constant := 8 * 1024; -- Bytes
   Max_Banks : constant := 16; -- 128 kB for MBC5 (16 x 8kByte)
   Max_Bytes : constant := Max_Banks * Bank_Size;

   subtype Bank_Count is Native_Unsigned range 0 .. Max_Banks;
   subtype Byte_Count is Native_Unsigned range 0 .. Max_Bytes;

   subtype Bank_Index is Bank_Count range 0 .. Max_Banks - 1;
   subtype Address is Byte_Count range 0 .. Max_Bytes - 1;

   type Content is array (Address range <>) of Byte;
   type Content_Access is access Content;

   procedure Load
     (Path : String;
      ROM  : out Content);

   procedure Save
     (Path : String;
      ROM  : Content);

end Gade.Cart.RAM;

