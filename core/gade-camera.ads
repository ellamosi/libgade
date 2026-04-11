with Interfaces;

package Gade.Camera is

   Capture_Height : constant := 112;
   Capture_Width  : constant := 128;

   subtype Column_Index is Natural range 0 .. Capture_Width - 1;
   subtype Pixel_Value is Interfaces.Unsigned_8 range 0 .. 3;
   subtype Row_Index is Natural range 0 .. Capture_Height - 1;

   type Bitmap is array (Row_Index, Column_Index) of Pixel_Value;

   type Provider_Interface is interface;

   procedure Capture_Frame (Provider : Provider_Interface; Frame : out Bitmap)
   is abstract;

   type Provider_Access is access all Provider_Interface'Class;

   function Default_Provider return Provider_Access;

   procedure Capture_Frame (Provider : Provider_Access; Frame : out Bitmap);

end Gade.Camera;
