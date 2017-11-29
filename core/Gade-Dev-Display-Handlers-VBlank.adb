with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;

package body Gade.Dev.Display.Handlers.VBlank is

   overriding
   procedure Reset
     (Mode_Handler : in out VBlank_Handler_Type)
   is
   begin
      Mode_Handler_Type (Mode_Handler).Reset;
      Mode_Handler.Remaining_Cycles := Mode_Cycles;
      Mode_Handler.Remaining_Line_Cycles := Line_Cycles;
   end Reset;

   overriding
   procedure Start
     (Mode_Handler : in out VBlank_Handler_Type;
      GB           : in out Gade.GB.GB_Type;
      Video        : RGB32_Display_Buffer_Access)
   is
   begin
      Mode_Handler_Type (Mode_Handler).Start (GB, Video);
      Mode_Handler.Dev.Frame_Finished := True;
      Set_Interrupt (GB, VBlank_Interrupt);
   end Start;

   overriding
   procedure Report_Cycles
     (Mode_Handler     : in out VBlank_Handler_Type;
      GB               : in out Gade.GB.GB_Type;
      Video            : RGB32_Display_Buffer_Access;
      Cycles           : Natural;
      Remaining_Cycles : out Natural)
   is
      pragma Unreferenced (Video);
      Remaining_Line_Cycles : Integer;
      New_Line              : Line_Count_Type;
   begin
      Remaining_Cycles := Cycles;
      while Remaining_Cycles > 0 and not Mode_Handler.Finished loop
         Remaining_Line_Cycles := Mode_Handler.Remaining_Line_Cycles - Remaining_Cycles;
         if Remaining_Line_Cycles <= 0 then
            --  Exhausted line cycles
            Remaining_Cycles := Remaining_Cycles - Mode_Handler.Remaining_Line_Cycles;
            if Mode_Handler.Display_Handler.Current_Line < Line_Count_Type'Last then
               Mode_Handler.Remaining_Line_Cycles := Line_Cycles;
               New_Line := Mode_Handler.Display_Handler.Current_Line + 1;
            else
               Mode_Handler.Finished := True;
               New_Line := 0;
            end if;
            Mode_Handler.Display_Handler.Line_Changed (GB, New_Line);
         else
            --  Not exhausted line cycles
            Remaining_Cycles := 0;
            Mode_Handler.Remaining_Line_Cycles := Remaining_Line_Cycles;
         end if;
      end loop;
   end Report_Cycles;

   overriding
   function Next_Mode
     (Mode_Handler : VBlank_Handler_Type) return LCD_Controller_Mode_Type is
      pragma Unreferenced (Mode_Handler);
   begin
      return Gade.Dev.Display.OAM_Access;
   end Next_Mode;

end Gade.Dev.Display.Handlers.VBlank;
