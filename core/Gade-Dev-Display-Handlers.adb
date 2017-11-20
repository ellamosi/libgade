with Gade.Dev.Display.Handlers.OAM_Access;  use Gade.Dev.Display.Handlers.OAM_Access;
with Gade.Dev.Display.Handlers.VRAM_Access; use Gade.Dev.Display.Handlers.VRAM_Access;
with Gade.Dev.Display.Handlers.HBlank;      use Gade.Dev.Display.Handlers.HBlank;
with Gade.Dev.Display.Handlers.VBlank;      use Gade.Dev.Display.Handlers.VBlank;

package body Gade.Dev.Display.Handlers is
   package Display_Modes renames Gade.Dev.Display;

   procedure Setup
     (Mode_Handler          : in out Mode_Handler_Type;
      Display_Handler       : Display_Handler_Access;
      Dev                   : access Display_Type)
   is
   begin
      Mode_Handler.Display_Handler := Display_Handler;
      Mode_Handler.Dev             := Dev;
   end Setup;

   procedure Reset
     (Mode_Handler : in out Mode_Handler_Type)
   is
   begin
      Mode_Handler.Finished := False;
   end Reset;

   procedure Start
     (Mode_Handler : in out Mode_Handler_Type;
      GB           : in out Gade.GB.GB_Type;
      Video        : RGB32_Display_Buffer_Access)
   is
      pragma Unreferenced (GB, Video);
   begin
      Reset (Mode_Handler_Type'Class (Mode_Handler));
   end Start;

   procedure Report_Cycles
     (Mode_Handler     : in out Mode_Handler_Type;
      GB               : in out Gade.GB.GB_Type;
      Video            : RGB32_Display_Buffer_Access;
      Cycles           : Natural;
      Remaining_Cycles : out Natural)
   is
      pragma Unreferenced (Video);
      Mode_Remaining_Cycles : Integer;
   begin
      Mode_Remaining_Cycles := Mode_Handler.Remaining_Cycles - Cycles;
      if Mode_Remaining_Cycles <= 0 then
         --  Exhausted mode cycles
         Remaining_Cycles := -Mode_Remaining_Cycles;
         Mode_Handler.Finished := True;
         Mode_Handler.Remaining_Cycles := 0;
         Mode_Finished (Mode_Handler_Type'Class (Mode_Handler), GB);
      else
         --  Not exhausted mode cycles
         Remaining_Cycles := 0;
         Mode_Handler.Remaining_Cycles := Mode_Remaining_Cycles;
      end if;
   end Report_Cycles;

   procedure Mode_Finished
     (Mode_Handler : in out Mode_Handler_Type;
      GB           : in out Gade.GB.GB_Type)
   is
   begin
      --  Meant to trigger in-mode events at the end of the mode such as line
      --  changes
      null;
   end Mode_Finished;

   function Is_Mode_Finished
     (Mode_Handler : Mode_Handler_Type) return Boolean
   is
   begin
      return Mode_Handler.Finished;
   end Is_Mode_Finished;

   procedure Line_Changed
     (Handler  : in out Display_Handler_Type;
      GB       : in out Gade.GB.GB_Type;
      New_Line : Line_Count_Type)
   is
   begin
      Handler.Current_Line := New_Line;
      Line_Changed (Handler.Dev.all, GB, New_Line);
   end Line_Changed;

   function Create (Dev : Display_Access) return Display_Handler_Access is
      HBlank_Handler      : constant HBlank_Handler_Access :=
        new HBlank_Handler_Type;
      VBlank_Handler      : constant VBlank_Handler_Access :=
        new VBlank_Handler_Type;
      OAM_Access_Handler  : constant OAM_Access_Handler_Access :=
        new OAM_Access_Handler_Type;
      VRAM_Access_Handler : constant VRAM_Access_Handler_Access :=
        new VRAM_Access_Handler_Type;

      Handler : constant Display_Handler_Access := new Display_Handler_Type;
   begin
      Handler.Dev := Dev;

      HBlank_Handler.Setup (Handler, Dev);
      VBlank_Handler.Setup (Handler, Dev);
      OAM_Access_Handler.Setup (Handler, Dev);
      VRAM_Access_Handler.Setup (Handler, Dev);

      Handler.Mode_Handlers :=
        (Display_Modes.HBlank      => Mode_Handler_Access (HBlank_Handler),
         Display_Modes.VBlank      => Mode_Handler_Access (VBlank_Handler),
         Display_Modes.OAM_Access  => Mode_Handler_Access (OAM_Access_Handler),
         Display_Modes.VRAM_Access => Mode_Handler_Access (VRAM_Access_Handler));

      return Handler;
   end Create;

   procedure Reset
     (Handler : in out Display_Handler_Type)
   is
   begin
      Handler.Current_Mode_Handler := Handler.Mode_Handlers (Starting_Mode);
      Handler.Current_Line := Starting_Line;
      Handler.Current_Mode_Handler.Reset;
      Handler.Mode := Display_Modes.OAM_Access;
   end Reset;

   procedure Report_Cycles
     (Handler : in out Display_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Video   : RGB32_Display_Buffer_Access;
      Cycles  : Natural)
   is
      Requested_Cycles : Natural;
      Remaining_Cycles : Natural := Cycles;
      Next_Mode        : LCD_Controller_Mode_Type;
   begin
      while Remaining_Cycles > 0 loop
         Requested_Cycles := Remaining_Cycles;
         Handler.Current_Mode_Handler.Report_Cycles
           (GB, Video, Requested_Cycles, Remaining_Cycles);
         if Handler.Current_Mode_Handler.Is_Mode_Finished then
            Next_Mode := Handler.Current_Mode_Handler.Next_Mode;
            Handler.Current_Mode_Handler := Handler.Mode_Handlers (Next_Mode);
            Handler.Mode := Next_Mode;
            Mode_Changed (Handler.Dev.all, GB, Next_Mode);
            Handler.Current_Mode_Handler.Start (GB, Video);
         end if;
      end loop;
   end Report_Cycles;

end Gade.Dev.Display.Handlers;
