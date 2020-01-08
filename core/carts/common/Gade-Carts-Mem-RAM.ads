package Gade.Carts.Mem.RAM is

   --  Some of these constants may change as support is added for different
   --  cartridge types.
   Max_RAM_Address_Bits : constant := 17; -- 128 KB RAM for MBC5
   Max_RAM_Content_Size : constant := 2 ** Max_RAM_Address_Bits;

   type RAM_Content_Size is range 0 .. Max_RAM_Content_Size;
   type RAM_Address is range 0 .. Max_RAM_Content_Size - 1;
   type RAM_Content is array (RAM_Address range <>) of aliased Byte;

   type RAM_Content_Access is access all RAM_Content;
   subtype RAM_Content_NN_Access is not null RAM_Content_Access;

   function Create
     (Reported_Size : RAM_Size_Type;
      Max_Size      : RAM_Content_Size) return RAM_Content_Access;

   function Create (Size : RAM_Content_Size) return RAM_Content_NN_Access;

   procedure Load
     (RAM  : out RAM_Content;
      File : File_Type);

   procedure Save
     (RAM  : RAM_Content;
      File : File_Type);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => RAM_Content, Name => RAM_Content_Access);

private

   package Common is new Mem.Common
     (Content_Size      => RAM_Content_Size,
      Address           => RAM_Address,
      Content           => RAM_Content,
      Content_NN_Access => RAM_Content_NN_Access);

   Content_Size_For_RAM_Size : constant array (RAM_Size_Type)
     of RAM_Content_Size :=
       (None        =>          0, -- Unused
        RAM_16kbit  =>   2 * 1024,
        RAM_64kbit  =>   8 * 1024,
        RAM_256kbit =>  32 * 1024,
        RAM_512kbit =>  64 * 1024,
        RAM_1Mbit   => 128 * 1024);

end Gade.Carts.Mem.RAM;
