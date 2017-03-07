with System;              use System;

with Gade.Dev.VRAM;       use Gade.Dev.VRAM;
with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;
with Gade.GB;             use Gade.GB;
with Gade.GB.Memory_Map;  use Gade.GB.Memory_Map;

package body Gade.Dev.Display is

   procedure Create (Display : out Display_Type) is
   begin
      Display.Reset;
   end Create;

   procedure Reset (Display : in out Display_Type) is
   begin
      Display.Line_Cycles := 0;
      Display.Mode_Cycles := 0;
      Display.Frame_Finished := False;
      Display.Map.LCDC := Default_LCD_Control;
      Display.Map.STAT := Default_LCD_Status;
      Display.Mode_Cycle_Limit :=
        Mode_Cycles(Display.Map.STAT.LCD_Controller_Mode);
      Display.Map.CURLINE := 0;
      Display.DMA_Copy_Ongoing := False;
   end Reset;

   procedure Read
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte) is
   begin
      Value := Display.Map.Space(Address);
   end Read;

   procedure Write
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : Byte) is
   begin
      if Display.Map.Space(Address)'Address = Display.Map.LCDC'Address then
         if (Value and 16#80#) /= 0 and not Display.Map.LCDC.LCD_Operation then
            -- TODO: Enable display
            null;
         elsif (Value and 16#80#) = 0 and Display.Map.LCDC.LCD_Operation then
            -- TODO: Disable display
            null;
         end if;
      elsif Display.Map.Space(Address)'Address = Display.Map.DMA'Address then
         Display.DMA_Source_Address := Word(Value) * 2**8;
         Display.DMA_Target_Address := OAM_IO_Address'First;
         Display.DMA_Clocks_Since_Last_Copy := -4; -- Setup clocks
      end if;
      Display.Map.Space(Address) := Value;
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
            Read_Byte(GB, Display.DMA_Source_Address, B);
            -- TODO: Optimize this by accessing directly to the OAM buffer?
            Write_Byte(GB, Display.DMA_Target_Address, B);
            Display.DMA_Source_Address := Display.DMA_Source_Address + 1;
            Display.DMA_Target_Address := Display.DMA_Target_Address + 1;
         end if;
      end if;
   end Do_DMA;

   procedure Write_Video_Buffer_Line
     (GB     : GB_Type;
      Buffer : RGB32_Display_Buffer_Access;
      Row    : Natural) is
   begin
      if Row in Display_Vertical_Range then
         for Col in Display_Horizontal_Range loop
            Buffer(Row, Col) :=
              Color_Lookup(Read_Screen_Pixel(GB, Col, Row));
         end loop;
      end if;
   end Write_Video_Buffer_Line;

   function Next_Mode
     (Display : Display_Type) return LCD_Controller_Mode_Type is
   begin
      case Display.Map.STAT.LCD_Controller_Mode is
         when HBlank =>
            return
              (if Natural(Display.Map.CURLINE) < Display_Vertical_Range'Last
               then OAM_Access
               else VBlank);
         when VBlank =>
            return
              (if Display.Map.LCDC.LCD_Operation
               then OAM_Access
               else HBlank);
         when OAM_Access =>
            return OAM_VRAM_Access;
         when OAM_VRAM_Access =>
            return HBlank;
      end case;
   end Next_Mode;

   procedure Next_Mode
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type) is
      New_Mode : LCD_Controller_Mode_Type;
      Mode_Interrupt : Boolean;
   begin
      New_Mode := Next_Mode(Display);
      Display.Map.STAT.LCD_Controller_Mode := New_Mode;
      Mode_Interrupt :=
        (case New_Mode is
            when HBlank     => Display.Map.STAT.Interrupt_HBlank,
            when VBlank     => Display.Map.STAT.Interrupt_VBlank,
            when OAM_Access => Display.Map.STAT.Interrupt_OAM_Access,
            when others     => False);
      Display.Mode_Cycle_Limit := Mode_Cycles(New_Mode);
      if Mode_Interrupt then
         Set_Interrupt(GB, LCDC_Interrupt);
      end if;
      if New_Mode = VBlank then
         Display.Frame_Finished := True;
         Set_Interrupt(GB, VBlank_Interrupt);
      end if;
   end Next_Mode;

   procedure Next_Line
     (Display : in out Display_Type;
      GB      : in out GB_Type;
      Video   : RGB32_Display_Buffer_Access) is
      Coincidence : Boolean;
   begin
      if Display.Map.LCDC.LCD_Operation then
         Write_Video_Buffer_Line(GB, Video, Natural(GB.Display.Map.CURLINE));
         Display.Map.CURLINE := Display.Map.CURLINE + 1;
         Coincidence :=
           Natural(Display.Map.CURLINE) = Natural(Display.Map.CMPLINE);
         Display.Map.STAT.Scanline_Coincidence := Coincidence;
         if Coincidence and Display.Map.STAT.Interrupt_Scanline_Coincidence then
            Set_Interrupt(GB, LCDC_Interrupt);
         end if;
      end if;
   end Next_Line;

   procedure Report_Cycle
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type;
      Video   : RGB32_Display_Buffer_Access) is
   begin
      Display.Mode_Cycles := Display.Mode_Cycles + 1;
      if Display.Mode_Cycles = Display.Mode_Cycle_Limit then
         Next_Mode(Display, GB);
         Display.Mode_Cycles := 0;
      end if;
      Display.Line_Cycles := Display.Line_Cycles + 1;
      if Display.Line_Cycles = 456 then
         Next_Line(Display, GB, Video);
         Display.Line_Cycles := 0;
      end if;
      if not Display.Map.LCDC.LCD_Operation and Display.Frame_Finished then
         Video.all := (others => (others => (255, 255, 255)));
      end if;

      Do_DMA(Display, GB);
   end Report_Cycle;

   function Read_Background_Pixel
     (GB   : GB_Type;
      X, Y : Natural) return Color_Value is
      Tile_Row, Tile_Col : Natural;
      Tile_PX_Row, Tile_PX_Col : Natural;
      Tile_Index : Byte; -- Should be something other than Byte perhaps?
      Tile_Line : Tile_Line_Type;
   begin
      Tile_Row := ((Y+Natural(GB.Display.Map.SCROLLY))/8) mod 32;
      Tile_PX_Row := (Y+Natural(GB.Display.Map.SCROLLY)) mod 8;
      Tile_Col := ((X+Natural(GB.Display.Map.SCROLLX))/8) mod 32;
      Tile_PX_Col := (X+Natural(GB.Display.Map.SCROLLX)) mod 8;

      -- TODO: Background_Tile_Map_Addr should be an enum
      if GB.Display.Map.LCDC.Background_Tile_Map_Addr then
         Tile_Index := GB.Video_RAM.Map.High_Tile_Map(Tile_Row, Tile_Col);
      else
         Tile_Index := GB.Video_RAM.Map.Low_Tile_Map(Tile_Row, Tile_Col);
      end if;

      -- This is a big no no, we are copying an entire Tile!
      if GB.Display.Map.LCDC.Tile_Data_Table_Addr then
         Tile_Line :=
           GB.Video_RAM.Map.Tile_Data.Low_Tile_Data(Tile_Index)(Tile_PX_Row);
      else
         Tile_Line :=
           GB.Video_RAM.Map.Tile_Data.High_Tile_Data(To_Signed(Tile_Index))(Tile_PX_Row);
      end if;

      return Tile_Color(Tile_Line, Tile_PX_Col);
   end Read_Background_Pixel;

   function Read_Sprite_Pixel
     (GB   : GB_Type;
      X, Y : Natural) return Sprite_Result_Type is
      PX_Row, PX_Col : Integer;
      Y_Limit    : Natural;
      Tile_Index : Byte; -- Should be something other than Byte perhaps?
      Tile_Line  : Tile_Line_Type;
      Result     : Sprite_Result_Type;
      Min_Vis_X  : Natural; -- Overlapping leftmost sprite has priority
      Sprite_X, Sprite_Y : Natural;
      PX_Val     : Color_Value;
   begin
      Min_Vis_X := Natural'Last;
      Y_Limit := (if GB.Display.Map.LCDC.Sprite_Size then 16 else 8);
      Result.Value := 0; -- Transparent TODO: Constant

      -- Iterate in reverse because the lowest sprite has priority if
      -- overlapping with another sprite in the same X position
      for Current_Sprite in reverse GB.Video_OAM.Map.Sprites'Range loop
         Sprite_X := Natural(GB.Video_OAM.Map.Sprites(Current_Sprite).X);
         Sprite_Y := Natural(GB.Video_OAM.Map.Sprites(Current_Sprite).Y);
         PX_Col := X - Sprite_X + 8;
         PX_Row := Y - Sprite_Y + 16;

         if PX_Row >= 0 and PX_Row < Y_Limit and PX_Col >= 0 and PX_Col < 8 then
            -- Sprite is in pixel

            if GB.Video_OAM.Map.Sprites(Current_Sprite).X_Flip then
               PX_Col := 7 - PX_Col;
            end if;
            if GB.Video_OAM.Map.Sprites(Current_Sprite).Y_Flip then
               PX_Row := Y_Limit - PX_Row - 1;
            end if;

            if GB.Display.Map.LCDC.Sprite_Size then
               Tile_Index := GB.Video_OAM.Map.Sprites(Current_Sprite).Pattern / 2;
               Tile_Line := GB.Video_RAM.Map.Tile_Data.Sprite_Data_8_16(Tile_Index)(PX_Row);
            else
               Tile_Index := GB.Video_OAM.Map.Sprites(Current_Sprite).Pattern;
               Tile_Line := GB.Video_RAM.Map.Tile_Data.Sprite_Data_8_8(Tile_Index)(PX_Row);
            end if;

            PX_Val := Tile_Color(Tile_Line, PX_Col);

            if PX_Val /= 0 and Min_Vis_X >= Sprite_X then
               Result.Value := PX_Val;
               -- TODO: Needs a better name or enum!
               Result.Priority := GB.Video_OAM.Map.Sprites(Current_Sprite).Priority;
               Result.Palette := GB.Video_OAM.Map.Sprites(Current_Sprite).Palette;
            end if;
         end if;
      end loop;
      return Result;
   end Read_Sprite_Pixel;

   function Read_Screen_Pixel
     (GB   : GB_Type;
      X, Y : Natural) return Color_Value is
      -- Coordinates should disappear and be based on internal Display state
      -- Then this should be called with the appropriate timing for each px
      -- Also save intermediate data that can be re-used between calls

      BG     : Color_Value;
      Sprite : Sprite_Result_Type;
      Result : Color_Value;
   begin
      if GB.Display.Map.LCDC.Sprite_Display then
         Sprite := Read_Sprite_Pixel(GB, X, Y);
      else
         Sprite.Value := 0;
      end if;

      if Sprite.Value = 0 or (Sprite.Value /= 0 and Sprite.Priority) then
         BG := Read_Background_Pixel(GB, X, Y);
      end if;

      if Sprite.Value /= 0 and
        (not Sprite.Priority or (Sprite.Priority and BG = 0))
      then
         case Sprite.Palette is
            when OBJ0PAL => Result := GB.Display.Map.OBJ0PAL(Sprite.Value);
            when OBJ1PAL => Result := GB.Display.Map.OBJ1PAL(Sprite.Value);
         end case;
      else
         Result := GB.Display.Map.BGRDPAL(BG);
      end if;

      return Result;
   end Read_Screen_Pixel;

   procedure Read_Screen_Buffer
     (GB     : GB_Type;
      Buffer : out Video_Buffer_Type) is
   begin
      for Screen_Row in Display_Vertical_Range loop
         for Screen_Column in Display_Horizontal_Range loop
            Buffer(Screen_Row, Screen_Column) :=
              Read_Screen_Pixel(GB, Screen_Column, Screen_Row);
         end loop;
      end loop;
   end Read_Screen_Buffer;

   procedure Read_Background
      (GB      : GB_Type;
       Map_High, Tile_High : Boolean;
       Buffer  : out Background_Buffer_Type) is
   begin
      -- TODO: Re-implement Tile source choice
      for Screen_Row in Display_Vertical_Range loop
         for Screen_Column in Display_Horizontal_Range loop
            Buffer(Screen_Row, Screen_Column) :=
              Read_Background_Pixel(GB, Screen_Column, Screen_Row);
         end loop;
      end loop;
   end Read_Background;

   procedure Read_Tiles
      (Display   : Display_Type;
       VRAM      : VRAM_Type;
       Tile_High : Boolean;
       Buffer    : out Tile_Buffer_Type) is
      Tile_Top, Tile_Left : Natural;
      Buffer_Row, Buffer_Col : Natural;
   begin
      for Tile_Index in VRAM.Map.Tile_Data.Low_Tile_Data'Range loop
         Tile_Top := (Natural(Tile_Index) / 16) * 8;
         Tile_Left := (Natural(Tile_Index) * 8) mod Tile_Buffer_Width;

         for Tile_Pixel_Row in 0..7 loop
            for Tile_Pixel_Column in 0..7 loop
               Buffer_Row := Tile_Top+Tile_Pixel_Row;
               Buffer_Col := Tile_Left+Tile_Pixel_Column;
               if not Tile_High then
                  Buffer(Buffer_Row, Buffer_Col) :=
                    Tile_Color
                      (VRAM.Map.Tile_Data.Low_Tile_Data
                         (Tile_Index)(Tile_Pixel_Row),
                       Tile_Pixel_Column);
               else
                  Buffer(Buffer_Row, Buffer_Col) :=
                    Tile_Color
                      (VRAM.Map.Tile_Data.High_Tile_Data
                         (To_Signed(Tile_Index))(Tile_Pixel_Row),
                       Tile_Pixel_Column);
               end if;
            end loop;
         end loop;
      end loop;
   end Read_Tiles;

   procedure Check_Frame_Finished
     (Display  : in out Display_Type;
      Finished : out Boolean) is
   begin
      Finished := Display.Frame_Finished;
      Display.Frame_Finished := False;
   end Check_Frame_Finished;

   function Read_Palettes (Display : Display_Type) return Palette_Info_Type is
      Result : Palette_Info_Type;
   begin
      Result.BGP  := Display.Map.BGRDPAL;
      Result.OBP0 := Display.Map.OBJ0PAL;
      Result.OBP1 := Display.Map.OBJ1PAL;
      return Result;
   end Read_Palettes;

end Gade.Dev.Display;
