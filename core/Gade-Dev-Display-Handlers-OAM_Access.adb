with Gade.GB; use Gade.GB;

package body Gade.Dev.Display.Handlers.OAM_Access is

   overriding
   procedure Reset
     (Mode_Handler : in out OAM_Access_Handler_Type)
   is
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
      Gade.Dev.Video.Sprites.Populate_Line_Cache
        (GB.Video_RAM,
         GB.Video_OAM,
         Mode_Handler.Display_Handler.Sprite_Cache,
         Sprite_Edge_Counts,
         Mode_Handler.Display_Handler.Current_Line,
         GB.Display.Map.LCDC.Sprite_Size);

      Find_VRAM_Access_Timings (Mode_Handler, Sprite_Edge_Counts);
      Mode_Handler_Type (Mode_Handler).Start (GB, Video);
   end Start;

   overriding
   function Next_Mode
     (Mode_Handler : OAM_Access_Handler_Type) return LCD_Controller_Mode_Type is
      pragma Unreferenced (Mode_Handler);
   begin
      return Gade.Dev.Display.VRAM_Access;
   end Next_Mode;

   package body Pixel_FIFO is

      procedure Reset (FIFO : out Pixel_FIFO_Type) is
      begin
         FIFO.N := 0;
      end Reset;

      procedure Push
        (FIFO  : in out Pixel_FIFO_Type)
      is
      begin
         FIFO.N := FIFO.N + 8;
      end Push;

      procedure Pop
        (FIFO  : in out Pixel_FIFO_Type)
      is
      begin
         FIFO.N := FIFO.N - 1;
      end Pop;

      function Is_Full (FIFO : Pixel_FIFO_Type) return Boolean is
      begin
         return FIFO.N >= Max_Pixels;
      end Is_Full;

      function Free_Pixels (FIFO : Pixel_FIFO_Type) return Natural is
      begin
         return Max_Pixels - FIFO.N;
      end Free_Pixels;

   end Pixel_FIFO;

   package body Tile_Fetcher is

      procedure Reset (Fetcher : out Tile_Fetcher_Type) is
      begin
         Fetcher.Steps := 0;
      end Reset;

      procedure Start_Fetching (Fetcher : in out Tile_Fetcher_Type) is
      begin
         Fetcher.Steps := 0;
      end Start_Fetching;

      procedure Step (Fetcher : in out Tile_Fetcher_Type) is
      begin
         Fetcher.Steps := Fetcher.Steps + 1;
      end Step;

      function Tile_Available (Fetcher : Tile_Fetcher_Type) return Boolean is
      begin
         --  Not really sure if tiles are ready to be fetched mid clock?!
         --  From the VRAM Access mode timings documented seems more likely that
         --  FIFO has to wait for the end of fetcher clock even if the fetcher
         --  had already fetched the tile and was waiting
         return Fetcher.Steps >= Required_Steps; -- and Fetcher.Steps mod 2 = 0;
      end Tile_Available;

   end Tile_Fetcher;

   use Pixel_FIFO, Tile_Fetcher;

   procedure Fetch_Column
     (Pixel_FIFO     : in out Pixel_FIFO_Type;
      Tile_Fetcher   : in out Tile_Fetcher_Type;
      Elapsed_Cycles : out Natural);

   procedure Fetch_Column
     (Pixel_FIFO     : in out Pixel_FIFO_Type;
      Tile_Fetcher   : in out Tile_Fetcher_Type;
      Elapsed_Cycles : out Natural)
   is
      Column_Fetched   : Boolean := False;
      Free_FIFO_Pixels : Natural;
      Tile_Ready       : Boolean;
   begin
      Elapsed_Cycles := 0;
      while not Column_Fetched loop
         Elapsed_Cycles := Elapsed_Cycles + 1;
         Free_FIFO_Pixels := Free_Pixels (Pixel_FIFO);
         Tile_Ready := Tile_Available (Tile_Fetcher);
         if Free_FIFO_Pixels >= 8 and Tile_Ready then
            Push (Pixel_FIFO);
            Start_Fetching (Tile_Fetcher);
            Pop (Pixel_FIFO);
            Step (Tile_Fetcher);
            Column_Fetched := True;
         elsif Free_FIFO_Pixels >= 8 and not Tile_Ready then
            --  Stall LCD
            Step (Tile_Fetcher);
         elsif Free_FIFO_Pixels < 8 then
            Pop (Pixel_FIFO);
            Step (Tile_Fetcher);
            Column_Fetched := True;
         end if;
      end loop;
   end Fetch_Column;

   procedure Find_VRAM_Access_Timings
     (Mode_Handler       : in out OAM_Access_Handler_Type;
      Sprite_Edge_Counts : Edge_Counts_Type)
   is
      Display : constant Display_Access := Mode_Handler.Display_Handler.Dev;

      Pixel_FIFO   : Pixel_FIFO_Type;
      Tile_Fetcher : Tile_Fetcher_Type;
      Total_Cycles : Integer := 0; -- Maybe -4? We seem to start off

      Sprite_Cycles : constant := 6;

      Window_X     : constant Integer := Integer (Display.Map.WNDPOSX);

      procedure Skip_Scroll_Pixels;
      procedure Calculate_Timing_Array;

      procedure Skip_Scroll_Pixels is
         Skept_Pixels : constant Natural := Integer (Display.Map.SCROLLX) mod 8;

         Pixel_Cycles : Natural;
         Sprite_Edges : Natural;
      begin
         --  Skept Pixels         | Actual Pixels
         --   (up to 7)           |
         --   0  1  2  3  4  5  6 |
         --  -7 -6 -5 -4 -3 -2 -1 | 0  1  2  3  4  5  6  7
         --                        =============== WNDX = 7 ===============
         --  =============== WNDX = 0 ===============
         for PX in 0 .. Skept_Pixels - 1 loop
            if PX = Window_X then Start_Fetching (Tile_Fetcher); end if;
            Sprite_Edges := Sprite_Edge_Counts (PX - 7);
            Fetch_Column (Pixel_FIFO, Tile_Fetcher, Pixel_Cycles);
            Total_Cycles :=
              Total_Cycles + Pixel_Cycles + Sprite_Edges * Sprite_Cycles;
         end loop;
      end Skip_Scroll_Pixels;

      procedure Calculate_Timing_Array is
         Pixel_Cycles : Natural;
         Sprite_Edges : Natural;
      begin
         for PX in Mode_Handler.Display_Handler.Timing_Cache'Range loop
            if PX = Window_X - 7 then Start_Fetching (Tile_Fetcher); end if;
            Sprite_Edges := Sprite_Edge_Counts (PX);
            Fetch_Column (Pixel_FIFO, Tile_Fetcher, Pixel_Cycles);
            Total_Cycles :=
              Total_Cycles + Pixel_Cycles + Sprite_Edges * Sprite_Cycles;
            Mode_Handler.Display_Handler.Timing_Cache (PX) := Total_Cycles;
         end loop;
      end Calculate_Timing_Array;
   begin
      Reset (Pixel_FIFO);
      Reset (Tile_Fetcher);

      Skip_Scroll_Pixels;
      Calculate_Timing_Array;
   end Find_VRAM_Access_Timings;

end Gade.Dev.Display.Handlers.OAM_Access;
