private package Gade.Dev.Display.Handlers.HBlank is

   type HBlank_Handler_Type is new Mode_Handler_Type with private;
   type HBlank_Handler_Access is access HBlank_Handler_Type;

   overriding
   procedure Reset
     (Mode_Handler : in out HBlank_Handler_Type);

   overriding
   procedure Start
     (Mode_Handler : in out HBlank_Handler_Type;
      GB           : in out Gade.GB.GB_Type;
      Video        : RGB32_Display_Buffer_Access);

   overriding
   procedure Mode_Finished
     (Mode_Handler : in out HBlank_Handler_Type;
      GB           : in out Gade.GB.GB_Type);

   overriding
   function Next_Mode
     (Mode_Handler : HBlank_Handler_Type) return LCD_Controller_Mode_Type;

private

   Mode_Cycles : constant := 204; -- 201-207 clks

   Display_Handler : Display_Handler_Access;

   type HBlank_Handler_Type is new Mode_Handler_Type with null record;

end Gade.Dev.Display.Handlers.HBlank;
