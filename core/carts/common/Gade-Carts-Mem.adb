package body Gade.Carts.Mem is

   package body Common is

      procedure Load
        (Mem  : out Content;
         File : File_Type)
      is
         Input_Stream : Stream_Access;
      begin
         Input_Stream := Ada.Streams.Stream_IO.Stream (File);
         Content'Read (Input_Stream, Mem);
      end Load;

      function Allocate (Size : Content_Size) return Content_NN_Access is
      begin
         return new Content (0 .. Last_Address (Size));
      end Allocate;

      function Last_Address (Size : Content_Size) return Address is
      begin
         return Address (Size - 1);
      end Last_Address;

   end Common;

end Gade.Carts.Mem;
