package body Gade.Carts.Mem.RAM is
   use Common;

   function Create
     (Reported_Size : RAM_Size_Type;
      Max_Size      : RAM_Content_Size) return RAM_Content_Access
   is
      Reported_Content_Size, Actual_Size : RAM_Content_Size;
   begin
      --  Only trust the header information to an extent, cap the content size
      --  to the maximum addressable by the controller.
      Reported_Content_Size := Content_Size_For_RAM_Size (Reported_Size);
      Actual_Size := RAM_Content_Size'Min (Reported_Content_Size, Max_Size);
      if Actual_Size > 0 then
         return Create (Actual_Size);
      else
         return null;
      end if;
   end Create;

   function Create (Size : RAM_Content_Size) return RAM_Content_NN_Access is
      Mem : constant RAM_Content_NN_Access := Allocate (Size);
   begin
      Mem.all := (others => Blank_Value);
      return Mem;
   end Create;

   procedure Load (RAM : out RAM_Content; File : File_Type) renames Common.Load;

   procedure Save
     (RAM  : RAM_Content;
      File : File_Type)
   is
      Output_Stream : Stream_Access;
   begin
      Output_Stream := Ada.Streams.Stream_IO.Stream (File);
      RAM_Content'Write (Output_Stream, RAM);
   end Save;

end Gade.Carts.Mem.RAM;
