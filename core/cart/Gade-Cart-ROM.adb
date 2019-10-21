with Ada.Directories;       use Ada.Directories;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO;

package body Gade.Cart.ROM is

   function Load (Path : String) return Content_Access is
      File         : File_Type;
      Input_Stream : Stream_Access;
      ROM_Size     : File_Size;
      ROM          : Content_Access;

      procedure Print_Banks;
      procedure Print_Banks is
         use Ada.Text_IO;
         Banks : constant Bank_Count := Bank_Count (ROM_Size / Bank_Size);
      begin
         for i in 0 .. Banks - 1 loop
            Put_Line ("Loading bank " & i'Img);
         end loop;
         Put_Line ("Loader: Successfully loaded" & Banks'Img & " ROM banks.");
      end Print_Banks;
   begin
      ROM_Size := Size (Path);
      --  TODO: Allocate a power of 2 regardless of actuall file size (ROM files could be trimmed)
      ROM := new Content (0 .. Address (ROM_Size - 1));
      Open (File, In_File, Path);
      Input_Stream := Ada.Streams.Stream_IO.Stream (File);
      Content'Read (Input_Stream, ROM.all);
      Close (File);
      Print_Banks; -- Temporary: to match expected test output
      return ROM;
   end Load;

end Gade.Cart.ROM;
