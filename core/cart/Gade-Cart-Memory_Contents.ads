package Gade.Cart.Memory_Contents is

   Bank_Size : constant := 16 * 1024; -- Bytes
   Max_Banks : constant := 512; -- 8 MB for MBC5 (512 x 16 kByte)
   Max_Bytes : constant := Max_Banks * Bank_Size;

   type Bank_Count is range 0 .. Max_Banks;
   subtype Content_Byte_Count is Native_Unsigned range 0 .. Max_Bytes;
   subtype Memory_Content_Address is Content_Byte_Count range 0 .. Max_Bytes - 1;
   subtype Memory_Content_Offset is Memory_Content_Address;

   type Memory_Content is array (Memory_Content_Address range <>) of Byte;

   type ROM_Content is new Memory_Content;

   type ROM_Content_Access is access ROM_Content;

   function Load (Path : String) return ROM_Content_Access;

   type RAM_Content is new Memory_Content;

   type RAM_Content_Access is access RAM_Content;

   procedure Load
     (Path : String;
      RAM  : out RAM_Content);

   procedure Save
     (Path : String;
      RAM  : RAM_Content);

end Gade.Cart.Memory_Contents;
