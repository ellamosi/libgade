limited with Gade.Dev.VRAM;
with Gade.Video_Buffer; use Gade.Video_Buffer;

package Gade.Dev.Video.Window is

   type Window_Result_Type is record
      Value   : Color_Value;
      Visible : Boolean;
   end record;

   function Read
     (VRAM      : Gade.Dev.VRAM.VRAM_Type;
      Row, Col  : Integer;
      Tile_Map  : Tile_Map_Access_Type;
      Tile_Data : Tile_Data_Access_Type) return Window_Result_Type;

end Gade.Dev.Video.Window;
