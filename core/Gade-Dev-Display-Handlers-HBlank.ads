private with Gade.Dev.Video.Window;

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
   use Gade.Dev.Video.Window;

   Mode_Cycles : constant := 204; -- 201-207 clks

   Display_Handler : Display_Handler_Access;

   type HBlank_Handler_Type is new Mode_Handler_Type with null record;

   procedure Write_Video_Buffer_Line
     (GB     : in out Gade.GB.GB_Type;
      Buffer : RGB32_Display_Buffer_Access;
      Row    : Natural);

   function Read_Screen_Pixel
     (GB   : in out Gade.GB.GB_Type;
      X, Y : Natural) return Color_Value;

   function Read_Window_Pixel
     (GB   : Gade.GB.GB_Type;
      X, Y : Natural) return Window_Result_Type;

   function Read_Background_Pixel
     (GB   : in out Gade.GB.GB_Type;
      X, Y : Natural) return Color_Value;

end Gade.Dev.Display.Handlers.HBlank;
