package body Gade.GB is

   procedure Create (GB : out GB_Type) is
   begin
      Display.Create (GB.Display);
      Reset (GB);
   end Create;

   procedure Reset (GB : in out GB_Type) is
   begin
      Reset (GB.CPU);
      --  if GB.Cart /= null then GB.Cart.Reset; end if;
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

end Gade.GB;
