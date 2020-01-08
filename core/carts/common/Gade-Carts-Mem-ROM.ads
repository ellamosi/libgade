with Ada.Directories; use Ada.Directories;

package Gade.Carts.Mem.ROM is

   --  Some of these constants may change as support is added for different
   --  cartridge types.
   Max_ROM_Address_Bits : constant := 23; -- 8MB ROM for MBC5
   Max_ROM_Content_Size : constant := 2 ** Max_ROM_Address_Bits;

   type ROM_Content_Size is range 0 .. Max_ROM_Content_Size;
   type ROM_Address is range 0 .. Max_ROM_Content_Size - 1;
   type ROM_Content is array (ROM_Address range <>) of aliased Byte;  --  TODO: Might not need to be aliased

   type ROM_Content_Access is access all ROM_Content;
   subtype ROM_Content_NN_Access is not null ROM_Content_Access;

   function Load (Path : String) return ROM_Content_Access;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ROM_Content, Name => ROM_Content_Access);

private

   package Common is new Mem.Common
     (Content_Size      => ROM_Content_Size,
      Address           => ROM_Address,
      Content           => ROM_Content,
      Content_NN_Access => ROM_Content_NN_Access);
   use Common;

   function Encompassing_Size (S : File_Size) return ROM_Content_Size;

   --  Commercial ROMs seem to be all at least 32kB, but there are a few
   --  small homebrew test ROMs that are just 16kB. Nonetheless, allow loading
   --  a bootstrap file too: 256B.
   Min_Size : constant ROM_Content_Size := 256;

end Gade.Carts.Mem.ROM;
