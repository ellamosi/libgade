package body Gade.Dev.Display.Handlers.VRAM_Access is

   overriding
   procedure Reset
     (Mode_Handler : in out VRAM_Access_Handler_Type) is
   begin
      Mode_Handler_Type (Mode_Handler).Reset;
      Mode_Handler.Remaining_Cycles := Mode_Cycles;
   end Reset;

   overriding
   function Next_Mode
     (Mode_Handler : VRAM_Access_Handler_Type) return LCD_Controller_Mode_Type
   is
      pragma Unreferenced (Mode_Handler);
   begin
      return Gade.Dev.Display.HBlank;
   end Next_Mode;

end Gade.Dev.Display.Handlers.VRAM_Access;
