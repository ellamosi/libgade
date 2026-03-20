with Image_IO;

with Testd.Sessions;

with Gade.Video_Buffer;

package Testd.Commands is
private
   use type Image_IO.Color_Info;
   use Gade.Video_Buffer;
   use Testd.Sessions;

   function To_Image
     (Buffer : RGB32_Display_Buffer) return Image_IO.Image_Data;

   function Image_Equal
     (Left : Image_IO.Image_Data; Right : Image_IO.Image_Data) return Boolean;

   procedure Ensure_Engine (S : in out Session);

   procedure Run_Frame (S : in out Session);

   function Ensure_ROM_Loaded (S : Session) return Boolean;

end Testd.Commands;
