with Gade.GB; use Gade.GB;

package body Gade.Dev.Display.Handlers.HBlank is

   overriding
   procedure Reset (Mode_Handler : in out HBlank_Handler_Type) is
   begin
      Mode_Handler_Type (Mode_Handler).Reset;
   end Reset;

   overriding
   procedure Start
     (Mode_Handler : in out HBlank_Handler_Type;
      GB           : in out Gade.GB.GB_Type;
      Video        : RGB32_Display_Buffer_Access) is
   begin
      Mode_Handler_Type (Mode_Handler).Start (GB, Video);
      --  TODO: Use proper names!
      Mode_Handler.Remaining_Cycles :=
        456 - 80 - Mode_Handler.Display_Handler.VRAM_Access_Cycles;
   end Start;

   overriding
   procedure Mode_Finished
     (Mode_Handler : in out HBlank_Handler_Type; GB : in out Gade.GB.GB_Type)
   is
      New_Line : Line_Count_Type;
   begin
      pragma Unreferenced (GB);

      if Mode_Handler.Display_Handler.Window_Line_Active then
         Mode_Handler.Display_Handler.Window_Line_Counter := @ + 1;
      end if;
      Mode_Handler.Display_Handler.Window_Line_Active := False;

      New_Line := Mode_Handler.Display_Handler.Current_Line + 1;
      Mode_Handler.Display_Handler.Pending_Line := New_Line;
      Mode_Handler.Display_Handler.Pending_Line_Valid := True;
   end Mode_Finished;

   overriding
   function Next_Mode (Mode_Handler : HBlank_Handler_Type) return LCD_Controller_Mode_Type
   is
   begin
      if Mode_Handler.Display_Handler.Current_Line < 143 then
         return Gade.Dev.Display.OAM_Access;
      else
         return Gade.Dev.Display.VBlank;
      end if;
   end Next_Mode;

end Gade.Dev.Display.Handlers.HBlank;
