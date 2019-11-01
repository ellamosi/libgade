with Ada.Directories;       use Ada.Directories;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO;

package body Gade.Carts.Memory_Contents is

   function Encompassing_ROM_Size (S : File_Size) return Content_Byte_Count is
      --  Find a power of 2 greater or equal than Min_ROM_Size that can fit the
      --  contents of the file. A ROM file could have been trimmed to exclude
      --  trailing zeroes or similar. Additionally, the user could be attempting
      --  to load something that's not really a ROM file, but can still attempt
      --  to run as if it were.
      Pow : Content_Byte_Count;
   begin
      Pow := Min_ROM_Size;
      while Content_Byte_Count (S) > Pow loop Pow := Pow * 2; end loop;
      return Pow;
   end Encompassing_ROM_Size;

   function Load (Path : String) return ROM_Content_Access is
      File          : File_Type;
      Input_Stream  : Stream_Access;
      ROM_File_Size : File_Size;
      ROM_Size      : Content_Byte_Count;
      ROM           : ROM_Content_Access;

      --  TODO: Update expected output from tests and remove this.
      procedure Print_Banks;
      procedure Print_Banks is
         use Ada.Text_IO;
         Banks : constant Bank_Count := Bank_Count (ROM_Size / ROM_Bank_Size);
      begin
         for i in 0 .. Banks - 1 loop
            Put_Line ("Loading bank " & i'Img);
         end loop;
         Put_Line ("Loader: Successfully loaded" & Banks'Img & " ROM banks.");
      end Print_Banks;
   begin
      ROM_File_Size := Size (Path);
      ROM_Size := Encompassing_ROM_Size (ROM_File_Size);
      ROM := new ROM_Content (0 .. ROM_Size - 1);
      Open (File, In_File, Path);
      Input_Stream := Ada.Streams.Stream_IO.Stream (File);
      ROM_Content'Read (Input_Stream, ROM.all);
      Close (File);
      Print_Banks; -- Temporary: to match expected test output
      return ROM;
   end Load;

   function Create
     (Reported_Size : RAM_Size_Type;
      Max_Size      : Content_Byte_Count) return RAM_Content_Access
   is
      Reported_Content_Size : Content_Byte_Count;
   begin
      --  Only trust the header information to an extent, cap the content size
      --  to the maximum addressable by the controller.
      Reported_Content_Size := Content_Size_For_RAM_Size (Reported_Size);
      return Create (Content_Byte_Count'Min (Reported_Content_Size, Max_Size));
   end Create;

   function Create (Size : Content_Byte_Count) return RAM_Content_NN_Access is
      Result : constant RAM_Content_NN_Access :=
         new RAM_Content (0 .. Size - 1);
   begin
      return Result;
   end Create;

   procedure Load
     (Path : String;
      RAM  : out RAM_Content)
   is
      File         : File_Type;
      Input_Stream : Stream_Access;
   begin
      Open (File, In_File, Path);
      Input_Stream := Ada.Streams.Stream_IO.Stream (File);
      RAM_Content'Read (Input_Stream, RAM);
      Close (File);
   exception
      when Ada.Streams.Stream_IO.Name_Error => RAM := (others => 16#FF#);
   end Load;

   procedure Save
     (Path : String;
      RAM  : RAM_Content)
   is
      File          : File_Type;
      Output_Stream : Stream_Access;
   begin
      Create (File, Out_File, Path);
      Output_Stream := Ada.Streams.Stream_IO.Stream (File);
      RAM_Content'Write (Output_Stream, RAM);
      Close (File);
   end Save;

end Gade.Carts.Memory_Contents;
