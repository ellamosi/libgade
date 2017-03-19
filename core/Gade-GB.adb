with Ada.Text_IO; use Ada.Text_IO;

package body Gade.GB is

   procedure Create (GB : out GB_Type) is
   begin
      Reset(GB);
   end Create;

   procedure Reset (GB : in out GB_Type) is
   begin
      Reset(GB.CPU);
      External_RAM.Reset(GB.External_RAM);
      VRAM.Reset(GB.Video_RAM);
      OAM.Reset(GB.Video_OAM);
      Joypad.Reset(GB.Joypad);
      Display.Reset(GB.Display);
      Reset(GB.Timer);
      Reset(GB.Interrupt_Flag);
      Reset(GB.Interrupt_Enable);
      GB.Content := (others => 0);
   end Reset;

   procedure Load_ROM
      (GB   : in out GB_Type;
       File : String) is
   begin
      Load_ROM (GB.External_ROM, File);
   end Load_ROM;

   procedure Report_Cycles
     (GB     : in out GB_Type;
      Video  : RGB32_Display_Buffer_Access;
      Cycles : Positive) is
   begin
      Report_Cycles(GB.Joypad, GB, Cycles);
      Report_Cycles(GB.Display, GB, Video, Cycles);
      Report_Cycles(GB.Timer, GB, Cycles);
   end Report_Cycles;

--
--     procedure Read_Screen_Buffer
--        (GameBoy : GameBoy_Type;
--         Buffer  : out Video_Buffer_Type) is
--     begin
--        Gade.Display.Read_Screen_Buffer
--           (GameBoy.GB.MM.Display,
--            GameBoy.GB.MM.Video_RAM,
--            GameBoy.GB.MM.Video_OAM,
--            Buffer);
--     end Read_Screen_Buffer;
--
--     procedure Read_Background
--        (GameBoy : GameBoy_Type;
--         Map_High, Tile_High : Boolean;
--         Buffer  : out Background_Buffer_Type) is
--     begin
--        Gade.Display.Read_Background
--           (GameBoy.GB.MM.Display,
--            GameBoy.GB.MM.Video_RAM,
--            Map_High, Tile_High,
--            Buffer);
--     end Read_Background;
--
--     procedure Read_Tiles
--        (GameBoy   : GameBoy_Type;
--         Tile_High : Boolean;
--         Buffer    : out Tile_Buffer_Type) is
--     begin
--        Gade.Display.Read_Tiles
--           (GameBoy.GB.MM.Display,
--            GameBoy.GB.MM.Video_RAM,
--            Tile_High,
--            Buffer);
--     end Read_Tiles;
--
--     function Read_Palettes (GameBoy : GameBoy_Type) return Palette_Info_Type is
--     begin
--        return Gade.Display.Read_Palettes(GameBoy.GB.MM.Display);
--     end Read_Palettes;

end Gade.GB;
