with Gade.GB; use Gade.GB;

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
      Sprite_Edge_Counts : Edge_Counts_Type;
   begin
      Gade.Dev.Video.Sprites.Populate_Line_Cache
        (GB.Video_RAM,
         GB.Video_OAM,
         Mode_Handler.Display_Handler.Sprite_Cache,
         Sprite_Edge_Counts,
         Mode_Handler.Display_Handler.Current_Line,
         GB.Display.Map.LCDC.Sprite_Size);

      Find_VRAM_Access_Timings (Mode_Handler, Sprite_Edge_Counts);
      Mode_Handler_Type (Mode_Handler).Start (GB, Video);
   end Start;

   overriding
   function Next_Mode
     (Mode_Handler : OAM_Access_Handler_Type) return LCD_Controller_Mode_Type is
      pragma Unreferenced (Mode_Handler);
   begin
      return Gade.Dev.Display.VRAM_Access;
   end Next_Mode;

   procedure Find_VRAM_Access_Timings
     (Mode_Handler       : in out OAM_Access_Handler_Type;
      Sprite_Edge_Counts : Edge_Counts_Type)
   is
      pragma Unreferenced (Sprite_Edge_Counts);
   begin
      for PX in Mode_Handler.Display_Handler.Timing_Cache'Range loop
         Mode_Handler.Display_Handler.Timing_Cache (PX) := PX + 3;
      end loop;
   end Find_VRAM_Access_Timings;

end Gade.Dev.Display.Handlers.OAM_Access;
