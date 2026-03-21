with Gade.Carts.Blank;

package body Gade.GB is

   procedure Create (GB : out GB_Type; Logger : Gade.Logging.Logger_Access) is
      use type Gade.Logging.Logger_Access;
   begin
      GB.Logger := (if Logger = null then Gade.Logging.Default_Logger else Logger);
      Display.Create (GB.Display);
      GB.Cart := Cart_Access (Gade.Carts.Blank.Singleton);
      Audio.Create (GB.Audio, GB.Logger);
      Reset (GB);
   end Create;

   procedure Reset (GB : in out GB_Type) is
   begin
      Reset (GB.CPU);
      GB.Cart.Reset;
      VRAM.Reset (GB.Video_RAM);
      OAM.Reset (GB.Video_OAM);
      Joypad.Reset (GB.Joypad);
      Display.Reset (GB.Display);
      Audio.Reset (GB.Audio);
      Reset (GB.Timer);
      Reset (GB.Interrupt_Flag);
      Reset (GB.Interrupt_Enable);
      GB.Content := [others => 0];
      Clear_Run_Context (GB);
   end Reset;

   procedure Set_Run_Context
     (GB    : in out GB_Type;
      Video : RGB32_Display_Buffer_Access;
      Audio : Audio_Buffer_Access) is
   begin
      pragma Assert (Video /= null);
      pragma Assert (Audio /= null);
      GB.Run_Context := (Video => Video, Audio => Audio);
   end Set_Run_Context;

   procedure Clear_Run_Context (GB : in out GB_Type) is
   begin
      GB.Run_Context := (Video => null, Audio => null);
   end Clear_Run_Context;

   procedure Tick_M_Cycles (GB : in out GB_Type; Cycles : M_Cycle_Count) is
      Video : constant RGB32_Display_Buffer_Access := GB.Run_Context.Video;
      Audio : constant Audio_Buffer_Access := GB.Run_Context.Audio;
   begin
      Report_Cycles (GB, Video, Audio, Cycles);
   end Tick_M_Cycles;

   procedure Report_Cycles
     (GB     : in out GB_Type;
      Video  : RGB32_Display_Buffer_Access;
      Audio  : Audio_Buffer_Access;
      Cycles : M_Cycle_Count) is
   begin
      Report_Cycles (GB.Joypad, GB, Cycles);
      Report_Cycles (GB.Display, GB, Video, Cycles);
      Report_Cycles (GB.Audio, Audio, Cycles);
      Report_Cycles (GB.Timer, GB, Cycles);
   end Report_Cycles;

   procedure Report_Frame
     (GB : in out GB_Type; Audio : Audio_Buffer_Access; Cycles : M_Cycle_Count) is
   begin
      GB.Cart.Report_Cycles (Cycles);
      Flush_Frame (GB.Audio, Audio, Cycles);
   end Report_Frame;

end Gade.GB;
