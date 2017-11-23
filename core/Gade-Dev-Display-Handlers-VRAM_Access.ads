private package Gade.Dev.Display.Handlers.VRAM_Access is

   type VRAM_Access_Handler_Type is new Mode_Handler_Type with private;
   type VRAM_Access_Handler_Access is access VRAM_Access_Handler_Type;

   overriding
   procedure Reset
     (Mode_Handler : in out VRAM_Access_Handler_Type);

   overriding
   procedure Report_Cycles
     (Mode_Handler     : in out VRAM_Access_Handler_Type;
      GB               : in out Gade.GB.GB_Type;
      Video            : RGB32_Display_Buffer_Access;
      Cycles           : Natural;
      Remaining_Cycles : out Natural);

   overriding
   function Is_Mode_Finished
     (Mode_Handler : VRAM_Access_Handler_Type) return Boolean;

   overriding
   function Next_Mode
     (Mode_Handler : VRAM_Access_Handler_Type) return LCD_Controller_Mode_Type;

private

   --  Mode_Cycles : constant := 172; -- 169-297 clks

   type VRAM_Access_Handler_Type is new Mode_Handler_Type with record
      Pixel_Cursor   : Natural;
      Mode_Cycles    : Natural;
   end record;

end Gade.Dev.Display.Handlers.VRAM_Access;
