with Gade.GB;                          use Gade.GB;
with Gade.Dev.OAM;                     use Gade.Dev.OAM;
with Gade.Dev.Video.Background_Buffer; use Gade.Dev.Video.Background_Buffer;

package body Gade.Dev.Display.Handlers.VRAM_Access is

   procedure Apply_Pixel_Display_State
     (Display_Handler : in out Display_Handler_Type; Phase : Natural);

   procedure Apply_Pixel_Display_State
     (Display_Handler : in out Display_Handler_Type; Phase : Natural) is
   begin
      for I in Display_Handler.Pending_Writes'Range loop
         if Display_Handler.Pending_Writes (I).Active
           and then Display_Handler.Pending_Writes (I).Phase <= Phase
         then
            Display_Handler.Latched_Map.Space
              (Display_Handler.Pending_Writes (I).Address) :=
              Display_Handler.Pending_Writes (I).Value;
            Display_Handler.Pending_Writes (I).Active := False;
         end if;
      end loop;
   end Apply_Pixel_Display_State;

   overriding
   procedure Reset (Mode_Handler : in out VRAM_Access_Handler_Type) is
      Display_Handler : constant Display_Handler_Access := Mode_Handler.Display_Handler;
   begin
      Mode_Handler_Type (Mode_Handler).Reset;
      Display_Handler.VRAM_Access_Cycles := 0;
      Display_Handler.Latched_Map := Display_Handler.Dev.Map;
      Display_Handler.Pending_Writes := [others => (others => <>)];
      Mode_Handler.Pixel_Cursor := 0;
      Mode_Handler.Mode_Cycles := Display_Handler.Timing_Cache (160 - 1);
      Mode_Handler.Scroll_X := Natural (Display_Handler.Dev.Map.SCROLLX);
   end Reset;

   overriding
   procedure Start
     (Mode_Handler : in out VRAM_Access_Handler_Type;
      GB           : in out Gade.GB.GB_Type;
      Video        : RGB32_Display_Buffer_Access)
   is
      Sprite_Edge_Counts : Edge_Counts_Type;
   begin
      Gade.Dev.Video.Sprites.Populate_Line_Cache
        (GB.Video_RAM,
         GB.Video_OAM,
         Mode_Handler.Display_Handler.Sprite_Cache,
         Sprite_Edge_Counts,
         Mode_Handler.Display_Handler.Current_Line,
         GB.Display.Map.LCDC.Sprite_Size);
      Mode_Handler_Type (Mode_Handler).Start (GB, Video);
   end Start;

   overriding
   procedure Report_Cycles
     (Mode_Handler     : in out VRAM_Access_Handler_Type;
      GB               : in out Gade.GB.GB_Type;
      Video            : RGB32_Display_Buffer_Access;
      Cycles           : Natural;
      Remaining_Cycles : out Natural)
   is
      pragma Unreferenced (Remaining_Cycles);

      Display_Handler : constant Display_Handler_Access := Mode_Handler.Display_Handler;

      Requested_Mode_Cycles : constant Natural :=
        Display_Handler.VRAM_Access_Cycles + Cycles;

      Run_Until_Cycles : constant Natural :=
        Natural'Min
          (Display_Handler.VRAM_Access_Cycles + Cycles, Mode_Handler.Mode_Cycles);

      Pixel_Cycles : Natural;
   begin
      while Display_Handler.VRAM_Access_Cycles < Run_Until_Cycles loop
         Pixel_Cycles := Display_Handler.Timing_Cache (Mode_Handler.Pixel_Cursor);
         if Pixel_Cycles <= Run_Until_Cycles then
            Apply_Pixel_Display_State (Display_Handler.all, Pixel_Cycles);
            Draw_Pixel
              (Mode_Handler,
               GB,
               Video,
               Display_Handler.Current_Line,
               Mode_Handler.Pixel_Cursor);
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
   function Is_Mode_Finished (Mode_Handler : VRAM_Access_Handler_Type) return Boolean is
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

   procedure Draw_Pixel
     (Mode_Handler : VRAM_Access_Handler_Type;
      GB           : in out GB_Type;
      Buffer       : RGB32_Display_Buffer_Access;
      Row          : Natural;
      Col          : Natural)
   is
      Color : Color_Value;
   begin
      Color := Read_Screen_Pixel (Mode_Handler, GB, Col, Row);
      Buffer (Row, Col) := Color_Lookup (Color);
   end Draw_Pixel;

   function Read_Screen_Pixel
     (Mode_Handler : VRAM_Access_Handler_Type; GB : in out GB_Type; X, Y : Natural)
      return Color_Value
   is
      --  Coordinates should disappear and be based on internal Display state
      --  Then this should be called with the appropriate timing for each px
      --  Also save intermediate data that can be re-used between calls

      BG     : Color_Value;
      Sprite : Sprite_Result_Type;
      Window : Window_Result_Type;
      Result : Color_Value;
   begin
      Window.Visible := False;
      if GB.Display.Display_Handler.Latched_Map.LCDC.Window_Display then
         Window := Read_Window_Pixel (GB, X, Y);
         if Window.Visible then
            GB.Display.Display_Handler.Window_Line_Active := True;
         end if;
      end if;

      if GB.Display.Display_Handler.Latched_Map.LCDC.Sprite_Display then
         Sprite := GB.Display.Display_Handler.Sprite_Cache (X);
      else
         Sprite.Value := Sprite_Transparent_Color;
      end if;

      if (Sprite.Value = Sprite_Transparent_Color
          or (Sprite.Value /= Sprite_Transparent_Color and Sprite.Priority = Behind_BG))
      then
         if not Window.Visible
           and GB.Display.Display_Handler.Latched_Map.LCDC.Background_Display
         then
            BG := Read_Background_Pixel (Mode_Handler, GB, X, Y);
         elsif Window.Visible then
            BG := Window.Value;
         else
            BG := 0;
         end if;
      end if;

      if Sprite.Value /= 0
        and (Sprite.Priority = Above_BG or (Sprite.Priority = Behind_BG and BG = 0))
      then
         case Sprite.Palette is
            when OBJ0PAL =>
               Result := GB.Display.Display_Handler.Latched_Map.OBJ0PAL (Sprite.Value);

            when OBJ1PAL =>
               Result := GB.Display.Display_Handler.Latched_Map.OBJ1PAL (Sprite.Value);
         end case;
      else
         Result := GB.Display.Display_Handler.Latched_Map.BGRDPAL (BG);
      end if;

      return Result;
   end Read_Screen_Pixel;

   function Read_Window_Pixel
     (GB : Gade.GB.GB_Type; X, Y : Natural) return Window_Result_Type
   is
      Window_Row, Window_Col : Integer;
      Result                 : Window_Result_Type;
   begin
      if Y < Natural (GB.Display.Display_Handler.Latched_Map.WNDPOSY) then
         Result.Visible := False;
         return Result;
      end if;

      Window_Row := Integer (GB.Display.Display_Handler.Window_Line_Counter);
      Window_Col := X - Natural (GB.Display.Display_Handler.Latched_Map.WNDPOSX) + 6;

      return
        Gade.Dev.Video.Window.Read
          (GB.Video_RAM,
           Window_Row,
           Window_Col,
           GB.Display.Display_Handler.Latched_Map.LCDC.Window_Tile_Table_Addr,
           GB.Display.Display_Handler.Latched_Map.LCDC.Tile_Data_Table_Addr);
   end Read_Window_Pixel;

   function Read_Background_Pixel
     (Mode_Handler : VRAM_Access_Handler_Type; GB : in out GB_Type; X, Y : Natural)
      return Color_Value is
   begin
      return
        Gade.Dev.Video.Background_Buffer.Read
          (GB.Video_RAM,
           (Y + Natural (GB.Display.Display_Handler.Latched_Map.SCROLLY)) mod 256,
           (X + Mode_Handler.Scroll_X) mod 256,
           GB.Display.Display_Handler.Latched_Map.LCDC.Background_Tile_Map_Addr,
           GB.Display.Display_Handler.Latched_Map.LCDC.Tile_Data_Table_Addr);
   end Read_Background_Pixel;

end Gade.Dev.Display.Handlers.VRAM_Access;
