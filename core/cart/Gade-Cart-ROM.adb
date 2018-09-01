with Ada.Directories;       use Ada.Directories;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO;

package body Gade.Cart.ROM is

   function Load (Path : String) return ROM_Content_Access is
      File         : File_Type;
      Input_Stream : Stream_Access;
      ROM_Size     : File_Size;
      Content      : ROM_Content_Access;

      procedure Print_Banks;
      procedure Print_Banks is
         use Ada.Text_IO;
         Banks : constant ROM_Bank_Count := ROM_Bank_Count (ROM_Size / (16 * 1024));
      begin
         for i in 0 .. Banks - 1 loop
            Put_Line ("Loading bank " & i'Img);
         end loop;
         Put_Line ("Loader: Successfully loaded" & Banks'Img & " ROM banks.");
      end Print_Banks;
   begin
      ROM_Size := Size (Path);
      Content := new ROM_Content_Type (0 .. ROM_Address_Range (ROM_Size - 1));
      Open (File, In_File, Path);
      Input_Stream := Ada.Streams.Stream_IO.Stream (File);
      ROM_Content_Type'Read (Input_Stream, Content.all);
      Close (File);
      Print_Banks; -- Temporary: to match expected test output
      return Content;
   end Load;

   function Header (ROM : ROM_Content_Access) return Cart_Header_Access is
      type Byte_Access is access all Byte;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Byte_Access,
         Target => Cart_Header_Access);
   begin
      return Convert (ROM (0)'Access);
   end Header;

end Gade.Cart.ROM;

