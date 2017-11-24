with System;              use System;

with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;
with Gade.GB;             use Gade.GB;
with Gade.GB.Memory_Map;  use Gade.GB.Memory_Map;
with Gade.Dev.OAM;        use Gade.Dev.OAM;
with Gade.Dev.Display.Handlers;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Dev.Display is

   procedure Create (Display : aliased out Display_Type) is
   begin
      Display.Display_Handler :=
        Display_Handler_Access
          (Gade.Dev.Display.Handlers.Create (Display'Unchecked_Access));
   end Create;

   overriding
   procedure Reset (Display : in out Display_Type) is
   begin
      Display.Frame_Finished := False;
      Display.Map.LCDC := Default_LCD_Control;
      Display.Map.STAT := Default_LCD_Status;
      Display.Map.CURLINE := Starting_Line;
      Display.DMA_Copy_Ongoing := False;
      Display.Current_Mode := Starting_Mode;
      Display.Display_Handler.Reset;
   end Reset;

   overriding
   procedure Read
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte) is
      pragma Unreferenced (GB);
   begin
      Value := Display.Map.Space (Address);
   end Read;

   overriding
   procedure Write
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : Byte) is
      pragma Unreferenced (GB);
      Old_Value : LCD_Control;
   begin
      if Display.Map.Space (Address)'Address = Display.Map.LCDC'Address then
         Old_Value := Display.Map.LCDC;
         Display.Map.Space (Address) := Value;
         --  Put_Line ("LCDC Write");
         if Display.Map.LCDC.LCD_Operation /= Old_Value.LCD_Operation then
            --  Enable/Disable display
            null;
         end if;
      elsif Display.Map.Space (Address)'Address = Display.Map.DMA'Address then
         Display.DMA_Source_Address := Word (Value) * 2**8;
         Display.DMA_Target_Address := OAM_IO_Address'First;
         Display.DMA_Clocks_Since_Last_Copy := -4; -- Setup clocks
      elsif Display.Map.Space (Address)'Address = Display.Map.CURLINE'Address then
         --  Reset the scanline rendering
         --  DO THIS
         --  Display.Line_Cycles := 0;
         null;
         --  Put_Line ("LY Write");
      --  elsif Display.Map.Space (Address)'Address = Display.Map.SCROLLY'Address then
      --   Put_Line ("SCY Write");
      end if;
      Display.Map.Space (Address) := Value;
   end Write;

   procedure Do_DMA
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type) is
      B : Byte;
   begin
      if Display.DMA_Target_Address in OAM_IO_Address then
         Display.DMA_Clocks_Since_Last_Copy :=
           Display.DMA_Clocks_Since_Last_Copy + 1;
         if Display.DMA_Clocks_Since_Last_Copy = 4 then
            Display.DMA_Clocks_Since_Last_Copy := 0;
            Read_Byte (GB, Display.DMA_Source_Address, B);
            --  TODO: Optimize this by accessing directly to the OAM buffer?
            --  Will definitely need to do this when implementing VRAM/OAM locks
            Write_Byte (GB, Display.DMA_Target_Address, B);
            Display.DMA_Source_Address := Display.DMA_Source_Address + 1;
            Display.DMA_Target_Address := Display.DMA_Target_Address + 1;
         end if;
      end if;
   end Do_DMA;

   procedure Report_Cycles
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type;
      Video   : RGB32_Display_Buffer_Access;
      Cycles  : Positive) is
   begin
      if Display.Map.LCDC.LCD_Operation then
         Gade.Dev.Display.Handlers.Report_Cycles
           (Display.Display_Handler.all,
            GB,
            Video,
            Cycles);
      elsif not Display.Map.LCDC.LCD_Operation and Display.Frame_Finished then
         --  Blank LCD when disabled, this might need to be revisited
         --  It could likely be implemented in a Disabled handler
         Video.all := (others => (others => (255, 255, 255)));
      end if;
      Do_DMA (Display, GB);
   end Report_Cycles;

   procedure Check_Frame_Finished
     (Display  : in out Display_Type;
      Finished : out Boolean) is
   begin
      Finished := Display.Frame_Finished;
      Display.Frame_Finished := False;
   end Check_Frame_Finished;

   procedure Mode_Changed
     (Display : in out Display_Type;
      GB      : in out GB_Type;
      Mode    : LCD_Controller_Mode_Type) is
      Mode_Interrupt : Boolean;
   begin
      Display.Map.STAT.LCD_Controller_Mode := Mode;
      Mode_Interrupt :=
        (case Display.Current_Mode is
            when HBlank     => Display.Map.STAT.Interrupt_HBlank,
            when VBlank     => Display.Map.STAT.Interrupt_VBlank,
            when OAM_Access => Display.Map.STAT.Interrupt_OAM_Access,
            when others     => False);
      if Mode_Interrupt then
         Set_Interrupt (GB, LCDC_Interrupt);
      end if;
   end Mode_Changed;

   procedure Line_Changed
     (Display : in out Display_Type;
      GB      : in out GB_Type;
      Line    : Line_Count_Type)
   is
      Coincidence : Boolean;
   begin
      --  Line changes are internally reported before mode changes by the
      --  handlers
      Display.Map.CURLINE := Line;
      Coincidence := Line = Natural (Display.Map.CMPLINE);
      Display.Map.STAT.Scanline_Coincidence := Coincidence;
--        Put_Line
--          ("YCMP:" & Display.Map.CMPLINE'Img & " LY:" & Display.Map.CURLINE'Img &
--           " Coincidence: " & Coincidence'Img);
      if Coincidence and Display.Map.STAT.Interrupt_Scanline_Coincidence then
         Set_Interrupt (GB, LCDC_Interrupt);
         --  Put_Line ("LYC Interrupt");
      end if;
   end Line_Changed;

end Gade.Dev.Display;
