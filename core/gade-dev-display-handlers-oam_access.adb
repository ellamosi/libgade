with Gade.GB; use Gade.GB;

package body Gade.Dev.Display.Handlers.OAM_Access is

   First_Pixel_Base_Cycles : constant := 11;
   Sprite_Fetch_Penalty    : constant := 6;
   Window_Start_Penalty    : constant := 6;

   function Is_Window_Visible_On_Line
     (Mode_Handler : OAM_Access_Handler_Type) return Boolean;

   function Window_Start_Column (Mode_Handler : OAM_Access_Handler_Type) return Integer;

   function Is_Window_Visible_On_Line
     (Mode_Handler : OAM_Access_Handler_Type) return Boolean
   is
      Current_Line : constant Natural := Mode_Handler.Display_Handler.Current_Line;
   begin
      return
        Mode_Handler.Dev.Map.LCDC.Window_Display
        and then Current_Line >= Natural (Mode_Handler.Dev.Map.WNDPOSY)
        and then Natural (Mode_Handler.Dev.Map.WNDPOSX) <= 166;
   end Is_Window_Visible_On_Line;

   function Window_Start_Column (Mode_Handler : OAM_Access_Handler_Type) return Integer is
   begin
      if not Is_Window_Visible_On_Line (Mode_Handler) then
         return Integer'Last;
      end if;

      return Integer (Mode_Handler.Dev.Map.WNDPOSX) - 7;
   end Window_Start_Column;

   overriding
   procedure Reset (Mode_Handler : in out OAM_Access_Handler_Type) is
   begin
      Mode_Handler_Type (Mode_Handler).Reset;
      Mode_Handler.Remaining_Cycles := Mode_Cycles;
   end Reset;

   overriding
   procedure Start
     (Mode_Handler : in out OAM_Access_Handler_Type;
      GB           : in out Gade.GB.GB_Type;
      Video        : RGB32_Display_Buffer_Access)
   is
      Sprite_Edge_Counts : Edge_Counts_Type;
   begin
      Mode_Handler.Display_Handler.Window_Line_Active := False;
      if GB.Display.Map.LCDC.Sprite_Display then
         Gade.Dev.Video.Sprites.Populate_Line_Cache
           (GB.Video_RAM,
            GB.Video_OAM,
            Mode_Handler.Display_Handler.Sprite_Cache,
            Sprite_Edge_Counts,
            Mode_Handler.Display_Handler.Current_Line,
            GB.Display.Map.LCDC.Sprite_Size);
      else
         --  With OBJ rendering disabled there are no sprite fetch stalls to
         --  fold into the mode-3 timing cache for this line.
         Sprite_Edge_Counts := [others => 0];
      end if;

      Find_VRAM_Access_Timings (Mode_Handler, Sprite_Edge_Counts);
      Mode_Handler_Type (Mode_Handler).Start (GB, Video);
   end Start;

   overriding
   function Next_Mode
     (Mode_Handler : OAM_Access_Handler_Type) return LCD_Controller_Mode_Type
   is
      pragma Unreferenced (Mode_Handler);
   begin
      return Gade.Dev.Display.VRAM_Access;
   end Next_Mode;

   procedure Find_VRAM_Access_Timings
     (Mode_Handler       : in out OAM_Access_Handler_Type;
      Sprite_Edge_Counts : Edge_Counts_Type)
   is
      Display_Handler        : constant Display_Handler_Access :=
        Mode_Handler.Display_Handler;
      Scroll_Discard_Cycles  : constant Natural :=
        Natural (Mode_Handler.Dev.Map.SCROLLX mod 8);
      First_Window_Column    : constant Integer := Window_Start_Column (Mode_Handler);
      Window_Penalty_Applied : Boolean := First_Window_Column < 0;
      Cycles                 : Natural := First_Pixel_Base_Cycles + Scroll_Discard_Cycles;
   begin
      if Window_Penalty_Applied then
         Cycles := Cycles + Window_Start_Penalty;
      end if;

      --  Sprite fetch stalls that occur before the first visible pixel still
      --  delay mode 3 startup and therefore need to be folded into the
      --  initial warmup.
      for Edge in Sprite_Edge_Counts'First .. -1 loop
         Cycles := Cycles + (Sprite_Fetch_Penalty * Sprite_Edge_Counts (Edge));
      end loop;

      for PX in Mode_Handler.Display_Handler.Timing_Cache'Range loop
         if not Window_Penalty_Applied and then PX = First_Window_Column then
            Cycles := Cycles + Window_Start_Penalty;
            Window_Penalty_Applied := True;
         end if;

         Cycles := Cycles + 1;
         Cycles := Cycles + (Sprite_Fetch_Penalty * Sprite_Edge_Counts (PX));
         Display_Handler.Timing_Cache (PX) := Cycles;
      end loop;
   end Find_VRAM_Access_Timings;

end Gade.Dev.Display.Handlers.OAM_Access;
