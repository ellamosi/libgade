with Gade.Cart.RAM; use Gade.Cart.RAM;

package Gade.Cart.Banked.RAM.MBC2 is

   type Handler_Type is new RAM.Handler_Type with private;
   type Handler_Access is access Handler_Type;

   procedure Initialize
     (Handler : out Handler_Type;
      Path    : String);

   overriding
   procedure Read
     (Handler : Handler_Type;
      Address : Bank_Address;
      Value   : out Byte);

   overriding
   procedure Write
     (Handler : in out Handler_Type;
      Address : Bank_Address;
      Value   : Byte);

   procedure Save
     (Handler : Handler_Type);

private

   type Path_Access is access constant String;

   Byte_Mask : constant Byte := 16#F0#;

   Max_Bytes : constant := 512;

   type Content is new Cart.RAM.Content (0 .. Max_Bytes - 1);
   type Content_Access is access Content;

   type Handler_Type is new RAM.Handler_Type with record
      RAM  : Content_Access;
      Path : Path_Access;
   end record;

end Gade.Cart.Banked.RAM.MBC2;
