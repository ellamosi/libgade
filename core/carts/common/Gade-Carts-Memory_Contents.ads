private with Ada.Directories;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package Gade.Carts.Memory_Contents is

   --  TODO: Separate ROM/RAM concerns

   --  TODO: This might need to go, ROM bank size is variable Plain VS Most MBC
   --  vs MBC6 for example. Is not really a ROM chip concern.
   ROM_Bank_Size : constant := 16 * 1024; -- Bytes
   RAM_Bank_Size : constant := 8 * 1024; -- Bytes

   Max_Banks     : constant := 512; -- 8 MB for MBC5 (512 x 16 kByte)
   Max_Bytes     : constant := Max_Banks * ROM_Bank_Size;

   type Bank_Count is range 0 .. Max_Banks;
   subtype Bank_Index_Range is Bank_Count range 0 .. Max_Banks - 1;

   subtype Content_Byte_Count is Native_Unsigned range 0 .. Max_Bytes;
   subtype Memory_Content_Address is Content_Byte_Count range 0 .. Max_Bytes - 1;
   subtype Memory_Content_Offset is Memory_Content_Address;

   type Memory_Content is array (Memory_Content_Address range <>) of aliased Byte;

   type ROM_Content is new Memory_Content;

   type ROM_Content_Access is access all ROM_Content;
   subtype ROM_Content_NN_Access is not null ROM_Content_Access;

   function Load (Path : String) return ROM_Content_Access;

   type RAM_Content is new Memory_Content;

   type RAM_Content_Access is access all RAM_Content;
   subtype RAM_Content_NN_Access is not null RAM_Content_Access;

   function Create
     (Reported_Size : RAM_Size_Type;
      Max_Size      : Content_Byte_Count) return RAM_Content_Access;

   function Create (Size : Content_Byte_Count) return RAM_Content_NN_Access;

   procedure Load
     (RAM  : out RAM_Content;
      File : File_Type);

   procedure Save
     (RAM  : RAM_Content;
      File : File_Type);

private
   use Ada.Directories;

   function Encompassing_ROM_Size (S : File_Size) return Content_Byte_Count;

   --  Commercial ROMs seem to be all at least 32kB, but there are a few
   --  small homebrew test ROMs that are just 16kB. Nonetheless, allow loading
   --  a bootstrap file too: 256B.
   Min_ROM_Size : constant Content_Byte_Count := 256;

   Content_Size_For_RAM_Size : constant array (RAM_Size_Type)
     of Content_Byte_Count :=
       (None        =>          0, -- Unused
        RAM_16kbit  =>   2 * 1024,
        RAM_64kbit  =>   8 * 1024,
        RAM_256kbit =>  32 * 1024,
        RAM_512kbit =>  64 * 1024,
        RAM_1Mbit   => 128 * 1024);

end Gade.Carts.Memory_Contents;