private package Gade.Dev.Display.Handlers.OAM_Access is

   type OAM_Access_Handler_Type is new Mode_Handler_Type with private;
   type OAM_Access_Handler_Access is access OAM_Access_Handler_Type;

   overriding
   procedure Reset
     (Mode_Handler : in out OAM_Access_Handler_Type);

   overriding
   procedure Start
     (Mode_Handler : in out OAM_Access_Handler_Type;
      GB           : in out Gade.GB.GB_Type;
      Video        : RGB32_Display_Buffer_Access);

   overriding
   function Next_Mode
     (Mode_Handler : OAM_Access_Handler_Type) return LCD_Controller_Mode_Type;

private

   Mode_Cycles : constant := 80; -- 77-83 clks

   type OAM_Access_Handler_Type is new Mode_Handler_Type with null record;

   procedure Find_VRAM_Access_Timings
     (Mode_Handler       : in out OAM_Access_Handler_Type;
      Sprite_Edge_Counts : Edge_Counts_Type);

end Gade.Dev.Display.Handlers.OAM_Access;
