with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package Gade.Carts.Mem is

private

   generic
      type Content_Size is range <>;
      type Address is range <>;
      type Content is array (Address range <>) of Byte;
      type Content_NN_Access is not null access Content;
   package Common is

      procedure Load
        (Mem  : out Content;
         File : File_Type);

      function Allocate (Size : Content_Size) return Content_NN_Access;

   private

      function Last_Address (Size : Content_Size) return Address;

   end Common;

end Gade.Carts.Mem;
