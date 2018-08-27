with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package body Gade.Cart.RAM is

   procedure Load
     (Path    : String;
      Content : out RAM_Content_Type)
   is
      File         : File_Type;
      Input_Stream : Stream_Access;
   begin
      Open (File, In_File, Path);
      Input_Stream := Ada.Streams.Stream_IO.Stream (File);
      RAM_Content_Type'Read (Input_Stream, Content);
      Close (File);
   exception
      when Name_Error => Content := (others => 0);
   end Load;

   procedure Save
     (Path    : String;
      Content : RAM_Content_Type)
   is
      File          : File_Type;
      Output_Stream : Stream_Access;
   begin
      Create (File, Out_File, Path);
      Output_Stream := Ada.Streams.Stream_IO.Stream (File);
      RAM_Content_Type'Write (Output_Stream, Content);
      Close (File);
   end Save;

end Gade.Cart.RAM;
