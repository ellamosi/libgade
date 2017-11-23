package body Gade.Dev.Display.Handlers.VRAM_Access is

   overriding
   procedure Reset
     (Mode_Handler : in out VRAM_Access_Handler_Type)
   is
      Display_Handler : constant Display_Handler_Access :=
        Mode_Handler.Display_Handler;
   begin
      Mode_Handler_Type (Mode_Handler).Reset;
      Display_Handler.VRAM_Access_Cycles := 0;
      Mode_Handler.Pixel_Cursor := 0;
      Mode_Handler.Mode_Cycles := Display_Handler.Timing_Cache (160 - 1);
   end Reset;

   overriding
   procedure Report_Cycles
     (Mode_Handler     : in out VRAM_Access_Handler_Type;
      GB               : in out Gade.GB.GB_Type;
      Video            : RGB32_Display_Buffer_Access;
      Cycles           : Natural;
      Remaining_Cycles : out Natural)
   is
      pragma Unreferenced (GB, Video, Remaining_Cycles);

      Display_Handler : constant Display_Handler_Access :=
        Mode_Handler.Display_Handler;

      Requested_Mode_Cycles : constant Natural :=
        Display_Handler.VRAM_Access_Cycles + Cycles;

      Run_Until_Cycles : constant Natural :=
        Natural'Min
          (Display_Handler.VRAM_Access_Cycles + Cycles,
           Mode_Handler.Mode_Cycles);

      Pixel_Cycles : Natural;
   begin
      while Display_Handler.VRAM_Access_Cycles < Run_Until_Cycles loop
         Pixel_Cycles := Display_Handler.Timing_Cache (Mode_Handler.Pixel_Cursor);
         if Pixel_Cycles <= Run_Until_Cycles then
            --  TODO: Draw Pixel
            Mode_Handler.Pixel_Cursor := Mode_Handler.Pixel_Cursor + 1;
            Display_Handler.VRAM_Access_Cycles := Pixel_Cycles;
         else
            --  Pixel not yet drawn, finished mid mode, mid pixel
            Display_Handler.VRAM_Access_Cycles := Run_Until_Cycles;
         end if;
      end loop;
      if Requested_Mode_Cycles > Run_Until_Cycles then
         Remaining_Cycles := Requested_Mode_Cycles - Run_Until_Cycles;
      else
         Remaining_Cycles := 0;
      end if;
   end Report_Cycles;

   overriding
   function Is_Mode_Finished
     (Mode_Handler : VRAM_Access_Handler_Type) return Boolean
   is
   begin
      return Mode_Handler.Pixel_Cursor >= 160;
   end Is_Mode_Finished;

   overriding
   function Next_Mode
     (Mode_Handler : VRAM_Access_Handler_Type) return LCD_Controller_Mode_Type
   is
      pragma Unreferenced (Mode_Handler);
   begin
      return Gade.Dev.Display.HBlank;
   end Next_Mode;

end Gade.Dev.Display.Handlers.VRAM_Access;
