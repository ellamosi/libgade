with Gade.GB; use Gade.GB;
with Gade.Dev.Video.Tile_Buffer;

package body Gade.Dev.Display.Handlers.OAM_Access is

   overriding
   procedure Reset
     (Mode_Handler : in out OAM_Access_Handler_Type)
   is
   begin
      Mode_Handler_Type (Mode_Handler).Reset;
      Mode_Handler.Remaining_Cycles := Mode_Cycles;
   end Reset;

   overriding
   procedure Start
     (Mode_Handler : in out OAM_Access_Handler_Type;
      GB           : in out Gade.GB.GB_Type;
      Video        : RGB32_Display_Buffer_Access)
   is
   begin
      Gade.Dev.Video.Sprites.Populate_Line_Cache
        (GB.Video_RAM,
         GB.Video_OAM,
         Mode_Handler.Display_Handler.Sprite_Cache,
         Mode_Handler.Display_Handler.Current_Line,
         GB.Display.Map.LCDC.Sprite_Size);
      --  This probably would be better gone, tile rasterization could happen
      --  on mem writes
      Gade.Dev.Video.Tile_Buffer.Rasterize_Tiles
        (GB.Video_RAM.Tile_Buffer, GB.Video_RAM);
      Mode_Handler_Type (Mode_Handler).Start (GB, Video);
   end Start;

   overriding
   function Next_Mode
     (Mode_Handler : OAM_Access_Handler_Type) return LCD_Controller_Mode_Type is
      pragma Unreferenced (Mode_Handler);
   begin
      return Gade.Dev.Display.VRAM_Access;
   end Next_Mode;

end Gade.Dev.Display.Handlers.OAM_Access;
