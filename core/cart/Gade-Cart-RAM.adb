with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package body Gade.Cart.RAM is

   procedure Load
     (Path : String;
      RAM  : out Content)
   is
      File         : File_Type;
      Input_Stream : Stream_Access;
   begin
      Open (File, In_File, Path);
      Input_Stream := Ada.Streams.Stream_IO.Stream (File);
      Content'Read (Input_Stream, RAM);
      Close (File);
   exception
      when Name_Error => RAM := (others => 16#FF#);
   end Load;

   procedure Save
     (Path : String;
      RAM  : Content)
   is
      File          : File_Type;
      Output_Stream : Stream_Access;
   begin
      Create (File, Out_File, Path);
      Output_Stream := Ada.Streams.Stream_IO.Stream (File);
      Content'Write (Output_Stream, RAM);
      Close (File);
   end Save;

end Gade.Cart.RAM;
