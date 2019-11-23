with Gade.Carts.Blank;

package body Gade.GB is

   procedure Create (GB : out GB_Type) is
   begin
      Display.Create (GB.Display);
      GB.Cart := Cart_Access (Gade.Carts.Blank.Singleton);
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
      Reset (GB.Timer);
      Reset (GB.Interrupt_Flag);
      Reset (GB.Interrupt_Enable);
      GB.Content := (others => 0);
   end Reset;

   procedure Report_Cycles
     (GB     : in out GB_Type;
      Video  : RGB32_Display_Buffer_Access;
      Cycles : Positive) is
   begin
      Report_Cycles (GB.Joypad, GB, Cycles);
      Report_Cycles (GB.Display, GB, Video, Cycles);
      Report_Cycles (GB.Timer, GB, Cycles);
   end Report_Cycles;

   procedure Report_Frame
     (GB     : in out GB_Type;
      Cycles : Positive)
   is
   begin
      GB.Cart.Report_Cycles (Cycles);
   end Report_Frame;

end Gade.GB;
