with Gade.GB;      use Gade.GB;
with Gade.Dev.OAM; use Gade.Dev.OAM;
with Gade.Dev.Video.Background_Buffer;

package body Gade.Dev.Display.Handlers.HBlank is

   overriding
   procedure Reset
     (Mode_Handler : in out HBlank_Handler_Type)
   is
   begin
      Mode_Handler_Type (Mode_Handler).Reset;
      Mode_Handler.Remaining_Cycles := Mode_Cycles;
   end Reset;

   overriding
   procedure Start
     (Mode_Handler : in out HBlank_Handler_Type;
      GB           : in out Gade.GB.GB_Type;
      Video        : RGB32_Display_Buffer_Access)
   is
   begin
      Write_Video_Buffer_Line
        (GB, Video, Mode_Handler.Display_Handler.Current_Line);
      Mode_Handler_Type (Mode_Handler).Start (GB, Video);
   end Start;

   overriding
   procedure Mode_Finished
     (Mode_Handler : in out HBlank_Handler_Type;
      GB           : in out Gade.GB.GB_Type)
   is
      New_Line : Line_Count_Type;
   begin
      New_Line := Mode_Handler.Display_Handler.Current_Line + 1;
      Mode_Handler.Display_Handler.Line_Changed (GB, New_Line);
   end Mode_Finished;

   overriding
   function Next_Mode
     (Mode_Handler : HBlank_Handler_Type) return LCD_Controller_Mode_Type is
   begin
      if Mode_Handler.Display_Handler.Current_Line < 144 then
         return Gade.Dev.Display.OAM_Access;
      else
         return Gade.Dev.Display.VBlank;
      end if;
   end Next_Mode;

   procedure Write_Video_Buffer_Line
     (GB     : in out GB_Type;
      Buffer : RGB32_Display_Buffer_Access;
      Row    : Natural)
   is
      Color : Color_Value;
   begin
      --  Put_Line ("Writing line " & Row'Img);
      for Col in Display_Horizontal_Range loop
         Color := Read_Screen_Pixel (GB, Col, Row);
         --  if Color /= 0 then Put_Line ("Non 0"); end if;
         Buffer (Row, Col) := Color_Lookup (Color);
      end loop;
   end Write_Video_Buffer_Line;

   function Read_Screen_Pixel
     (GB   : in out GB_Type;
      X, Y : Natural) return Color_Value is
      --  Coordinates should disappear and be based on internal Display state
      --  Then this should be called with the appropriate timing for each px
      --  Also save intermediate data that can be re-used between calls

      BG      : Color_Value;
      Sprite  : Sprite_Result_Type;
      Window  : Window_Result_Type;
      Result  : Color_Value;
   begin
      Window.Visible := False;

      if GB.Display.Map.LCDC.Sprite_Display then
         Sprite := GB.Display.Display_Handler.Sprite_Cache (X);
      else
         Sprite.Value := Sprite_Transparent_Color;
      end if;

      if (Sprite.Value = Sprite_Transparent_Color or
         (Sprite.Value /= Sprite_Transparent_Color and Sprite.Priority = Behind_BG))
      then
         if GB.Display.Map.LCDC.Window_Display then
            Window := Read_Window_Pixel (GB, X, Y);
         end if;

         if not Window.Visible then
            BG := Read_Background_Pixel (GB, X, Y);
         else
            BG := Window.Value;
         end if;
      end if;

      BG := Read_Background_Pixel (GB, X, Y);

      if Sprite.Value /= 0 and
        (Sprite.Priority = Above_BG or (Sprite.Priority = Behind_BG and BG = 0))
      then
         case Sprite.Palette is
            when OBJ0PAL => Result := GB.Display.Map.OBJ0PAL (Sprite.Value);
            when OBJ1PAL => Result := GB.Display.Map.OBJ1PAL (Sprite.Value);
         end case;
      else
         Result := GB.Display.Map.BGRDPAL (BG);
      end if;

      return Result;
   end Read_Screen_Pixel;

   function Read_Window_Pixel
     (GB   : Gade.GB.GB_Type;
      X, Y : Natural) return Window_Result_Type is
      Window_Row, Window_Col : Integer;
   begin
      Window_Row := Y - Natural (GB.Display.Map.WNDPOSY);
      Window_Col := X - Natural (GB.Display.Map.WNDPOSX) + 6;

      return
        Gade.Dev.Video.Window.Read
          (GB.Video_RAM,
           Window_Row,
           Window_Col,
           GB.Display.Map.LCDC.Window_Tile_Table_Addr,
           GB.Display.Map.LCDC.Tile_Data_Table_Addr);
   end Read_Window_Pixel;

   function Read_Background_Pixel
     (GB   : in out GB_Type;
      X, Y : Natural) return Color_Value is
   begin
      return
        Gade.Dev.Video.Background_Buffer.Read
          (GB.Video_RAM,
           (Y + Natural (GB.Display.Map.SCROLLY)) mod 256,
           (X + Natural (GB.Display.Map.SCROLLX)) mod 256,
           GB.Display.Map.LCDC.Background_Tile_Map_Addr,
           GB.Display.Map.LCDC.Tile_Data_Table_Addr);
   end Read_Background_Pixel;

end Gade.Dev.Display.Handlers.HBlank;
