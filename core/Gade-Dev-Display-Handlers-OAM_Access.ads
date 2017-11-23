private package Gade.Dev.Display.Handlers.OAM_Access is

   type OAM_Access_Handler_Type is new Mode_Handler_Type with private;
   type OAM_Access_Handler_Access is access OAM_Access_Handler_Type;

   overriding
   procedure Reset
     (Mode_Handler : in out OAM_Access_Handler_Type);

   overriding
   procedure Start
     (Mode_Handler : in out OAM_Access_Handler_Type;
      GB           : in out Gade.GB.GB_Type;
      Video        : RGB32_Display_Buffer_Access);

   overriding
   function Next_Mode
     (Mode_Handler : OAM_Access_Handler_Type) return LCD_Controller_Mode_Type;

private

   Mode_Cycles : constant := 80; -- 77-83 clks

   type OAM_Access_Handler_Type is new Mode_Handler_Type with null record;

   --  TODO: This should go to its own file
   package Pixel_FIFO is

      type Pixel_FIFO_Type is private;

      procedure Reset (FIFO : out Pixel_FIFO_Type);

      --  Pushes 8 pixels
      procedure Push
        (FIFO  : in out Pixel_FIFO_Type);

      --  Pops 1 pixel
      procedure Pop
        (FIFO  : in out Pixel_FIFO_Type);

      function Is_Full (FIFO : Pixel_FIFO_Type) return Boolean;

      function Free_Pixels (FIFO : Pixel_FIFO_Type) return Natural;

   private

      Min_Pixels : constant := 8;
      Max_Pixels : constant := 16;

      type Pixel_FIFO_Type is record
         N : Natural;
      end record;

   end Pixel_FIFO;

   package Tile_Fetcher is

      type Tile_Fetcher_Type is private;

      procedure Reset (Fetcher : out Tile_Fetcher_Type);

      procedure Start_Fetching (Fetcher : in out Tile_Fetcher_Type);

      procedure Step (Fetcher : in out Tile_Fetcher_Type);

      function Tile_Available (Fetcher : Tile_Fetcher_Type) return Boolean;

   private

      Required_Steps : constant := 6;

      type Tile_Fetcher_Type is record
         Steps : Integer;
      end record;

   end Tile_Fetcher;

   procedure Find_VRAM_Access_Timings
     (Mode_Handler       : in out OAM_Access_Handler_Type;
      Sprite_Edge_Counts : Edge_Counts_Type);

end Gade.Dev.Display.Handlers.OAM_Access;
