private package Gade.Dev.Display.Handlers.VRAM_Access is

   type VRAM_Access_Handler_Type is new Mode_Handler_Type with private;
   type VRAM_Access_Handler_Access is access VRAM_Access_Handler_Type;

   overriding
   procedure Reset
     (Mode_Handler : in out VRAM_Access_Handler_Type);

   overriding
   function Next_Mode
     (Mode_Handler : VRAM_Access_Handler_Type) return LCD_Controller_Mode_Type;

private

   Mode_Cycles : constant := 172; -- 169-297 clks

   type VRAM_Access_Handler_Type is new Mode_Handler_Type with null record;

end Gade.Dev.Display.Handlers.VRAM_Access;
