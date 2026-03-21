package body Gade.Carts.Mem.ROM is

   function Encompassing_Size (S : File_Size) return ROM_Content_Size is
      --  Find a power of 2 greater or equal than Min_ROM_Size that can fit the
      --  contents of the file. A ROM file could have been trimmed to exclude
      --  trailing zeroes or similar. Additionally, the user could be attempting
      --  to load something that's not really a ROM file, but can still attempt
      --  to run as if it were.
      Pow : ROM_Content_Size;
   begin
      Pow := Min_Size;
      while ROM_Content_Size (S) > Pow loop
         Pow := Pow * 2;
      end loop;
      return Pow;
   end Encompassing_Size;

   function Load (Path : String) return ROM_Content_Access is
      File   : File_Type;
      F_Size : File_Size;
      Size   : ROM_Content_Size;
      Mem    : ROM_Content_Access;
   begin
      F_Size := Ada.Directories.Size (Path);
      Size := Encompassing_Size (F_Size);
      Mem := Allocate (Size);
      Open (File, In_File, Path);
      Load (Mem.all, File);
      Close (File);
      return Mem;
   end Load;

end Gade.Carts.Mem.ROM;
