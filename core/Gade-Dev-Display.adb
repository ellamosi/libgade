with System;              use System;

with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;
with Gade.GB;             use Gade.GB;
with Gade.GB.Memory_Map;  use Gade.GB.Memory_Map;
with Gade.Dev.Video.Tile_Buffer; use Gade.Dev.Video.Tile_Buffer;
with Gade.Dev.Video.Background_Buffer;

with Gade.Dev.OAM; use Gade.Dev.OAM;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Dev.Display is

   procedure Create (Display : out Display_Type) is
   begin
      Display.Reset;
   end Create;

   overriding
   procedure Reset (Display : in out Display_Type) is
   begin
      Display.Line_Cycles := 80 + 172;
      Display.Mode_Cycles := 0;
      Display.Frame_Finished := False;
      Display.Map.LCDC := Default_LCD_Control;
      Display.Map.STAT := Default_LCD_Status;
      Display.Mode_Cycle_Limit :=
        Mode_Cycles (Display.Map.STAT.LCD_Controller_Mode);
      Display.Map.CURLINE := 0;
      Display.DMA_Copy_Ongoing := False;
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
         Display.Line_Cycles := 0;
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
            Write_Byte (GB, Display.DMA_Target_Address, B);
            Display.DMA_Source_Address := Display.DMA_Source_Address + 1;
            Display.DMA_Target_Address := Display.DMA_Target_Address + 1;
         end if;
      end if;
   end Do_DMA;

   procedure Write_Video_Buffer_Line
     (GB     : in out GB_Type;
      Buffer : RGB32_Display_Buffer_Access;
      Row    : Natural) is
   begin
      if Row in Display_Vertical_Range then
         for Col in Display_Horizontal_Range loop
            Buffer (Row, Col) :=
              Color_Lookup (Read_Screen_Pixel (GB, Col, Row));
         end loop;
      end if;
   end Write_Video_Buffer_Line;

   function Next_Mode
     (Display : Display_Type) return LCD_Controller_Mode_Type is
   begin
      case Display.Map.STAT.LCD_Controller_Mode is
         when HBlank =>
            return
              (if Natural (Display.Map.CURLINE) < Display_Vertical_Range'Last
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
      GB      : in out Gade.GB.GB_Type;
      Video   : RGB32_Display_Buffer_Access) is
      New_Mode : LCD_Controller_Mode_Type;
      Mode_Interrupt : Boolean;
   begin
      New_Mode := Next_Mode (Display);
      Display.Map.STAT.LCD_Controller_Mode := New_Mode;
      Mode_Interrupt :=
        (case New_Mode is
            when HBlank     => Display.Map.STAT.Interrupt_HBlank,
            when VBlank     => Display.Map.STAT.Interrupt_VBlank,
            when OAM_Access => Display.Map.STAT.Interrupt_OAM_Access,
            when others     => False);
      Display.Mode_Cycle_Limit := Mode_Cycles (New_Mode);
      if Mode_Interrupt then
         Set_Interrupt (GB, LCDC_Interrupt);
      end if;
      if New_Mode = VBlank then
         Display.Frame_Finished := True;
         Set_Interrupt (GB, VBlank_Interrupt);
      elsif New_Mode = HBlank then
         Write_Video_Buffer_Line (GB, Video, Natural (GB.Display.Map.CURLINE));
      end if;
   end Next_Mode;

   procedure Next_Line
     (Display : in out Display_Type;
      GB      : in out GB_Type) is
      Coincidence : Boolean;
   begin
      if Display.Map.LCDC.LCD_Operation then
         Display.Map.CURLINE := Display.Map.CURLINE + 1;
         Gade.Dev.Video.Tile_Buffer.Rasterize_Tiles
           (GB.Video_RAM.Tile_Buffer, GB.Video_RAM);
         Coincidence :=
           Natural (Display.Map.CURLINE) = Natural (Display.Map.CMPLINE);
         Display.Map.STAT.Scanline_Coincidence := Coincidence;
         if Coincidence and Display.Map.STAT.Interrupt_Scanline_Coincidence then
            Set_Interrupt (GB, LCDC_Interrupt);
         end if;
         --  Put_Line ("Line:" & Integer'Image (Integer (Display.Map.CURLINE)) &
         --  " Next_Line Mode:" & Display.Map.STAT.LCD_Controller_Mode'Img &
         --  " Mode cycles:" & Display.Mode_Cycles'Image);
         if Natural (Display.Map.CURLINE) in Display_Vertical_Range then
            --  Put_Line ("Line:" & Integer'Image (Integer (Display.Map.CURLINE)));
            Gade.Dev.Video.Sprites.Populate_Line_Cache
              (GB.Video_RAM,
               GB.Video_OAM,
               Display.Sprite_Cache,
               Natural (Display.Map.CURLINE),
               GB.Display.Map.LCDC.Sprite_Size);
         end if;
      end if;
   end Next_Line;

   procedure Start_OAM_Access
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type)
   is
   begin
      --  TODO: New Display state machine implementation
      null;
   end Start_OAM_Access;

   procedure Do_OAM_Access
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type)
   is
   begin
      --  TODO: New Display state machine implementation
      null;
   end Do_OAM_Access;

   procedure Start_OAM_VRAM_Access
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type)
   is
   begin
      --  TODO: New Display state machine implementation
      null;
   end Start_OAM_VRAM_Access;

   procedure Do_OAM_VRAM_Access
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type)
   is
   begin
      --  TODO: New Display state machine implementation
      null;
   end Do_OAM_VRAM_Access;

   procedure Start_HBlank
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type)
   is
   begin
      --  TODO: New Display state machine implementation
      null;
   end Start_HBlank;

   procedure Do_HBlank
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type)
   is
   begin
      --  TODO: New Display state machine implementation
      null;
   end Do_HBlank;

   procedure Start_VBlank
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type)
   is
   begin
      --  TODO: New Display state machine implementation
      null;
   end Start_VBlank;

   procedure Do_VBlank
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type)
   is
   begin
      --  TODO: New Display state machine implementation
      null;
   end Do_VBlank;

   procedure Report_Cycles
     (Display : in out Display_Type;
      GB      : in out Gade.GB.GB_Type;
      Video   : RGB32_Display_Buffer_Access;
      Cycles  : Positive) is
   begin
      if Display.Map.LCDC.LCD_Operation then
         case Display.Map.STAT.LCD_Controller_Mode is
            when OAM_Access      => Do_OAM_Access (Display, GB);
            when OAM_VRAM_Access => Do_OAM_VRAM_Access (Display, GB);
            when HBlank          => Do_HBlank (Display, GB);
            when VBlank          => Do_VBlank (Display, GB);
         end case;
      else
         null;
      end if;

      Display.Mode_Cycles := Display.Mode_Cycles + Cycles;
      if Display.Mode_Cycles >= Display.Mode_Cycle_Limit then
         Display.Mode_Cycles := Display.Mode_Cycles - Display.Mode_Cycle_Limit;
         Next_Mode (Display, GB, Video);
      end if;

      Display.Line_Cycles := Display.Line_Cycles + Cycles;
      if Display.Line_Cycles >= 456 then
         Display.Line_Cycles := Display.Line_Cycles - 456;
         Next_Line (Display, GB);
      end if;
      if not Display.Map.LCDC.LCD_Operation and Display.Frame_Finished then
         Video.all := (others => (others => (255, 255, 255)));
      end if;

      Do_DMA (Display, GB);
   end Report_Cycles;

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
         Sprite := GB.Display.Sprite_Cache (X);
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
