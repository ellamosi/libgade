private package Gade.Dev.Display.Handlers.VBlank is

   type VBlank_Handler_Type is new Mode_Handler_Type with private;
   type VBlank_Handler_Access is access VBlank_Handler_Type;

   overriding
   procedure Reset
     (Mode_Handler : in out VBlank_Handler_Type);

   overriding
   procedure Start
     (Mode_Handler : in out VBlank_Handler_Type;
      GB           : in out Gade.GB.GB_Type;
      Video        : RGB32_Display_Buffer_Access);

   overriding
   procedure Report_Cycles
     (Mode_Handler     : in out VBlank_Handler_Type;
      GB               : in out Gade.GB.GB_Type;
      Video            : RGB32_Display_Buffer_Access;
      Cycles           : Natural;
      Remaining_Cycles : out Natural);

   overriding
   function Next_Mode
     (Mode_Handler : VBlank_Handler_Type) return LCD_Controller_Mode_Type;

private

   Mode_Cycles : constant := 4560;
   Line_Cycles : constant := 456;
   Mode_Lines  : constant := 10;

   type VBlank_Handler_Type is new Mode_Handler_Type with record
      Remaining_Line_Cycles : Natural;
   end record;

end Gade.Dev.Display.Handlers.VBlank;
