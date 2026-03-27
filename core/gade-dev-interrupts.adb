with Gade.Dev.CPU;     use Gade.Dev.CPU;
with Gade.Dev.CPU.Instructions;
with Gade.Dev.Display; use Gade.Dev.Display;
with Gade.GB;          use Gade.GB;

package body Gade.Dev.Interrupts is

   Interrupt_Register_Read_Mask : constant Byte := 16#E0#;

   Interrupt_Bits : constant array (Interrupt_Type) of Byte :=
     [VBlank_Interrupt => 2#0000_0001#,
      LCDC_Interrupt   => 2#0000_0010#,
      Timer_Interrupt  => 2#0000_0100#,
      Serial_Interrupt => 2#0000_1000#,
      Joypad_Interrupt => 2#0001_0000#];

   function Pending_Interrupt_Mask (GB : Gade.GB.GB_Type) return Byte;

   function Highest_Priority_Interrupt (Mask : Byte) return Interrupt_Type;

   function Pending_Interrupt_Mask (GB : Gade.GB.GB_Type) return Byte is
   begin
      return
        (GB.Interrupt_Enable.Map.Reg and GB.Interrupt_Flag.Map.Reg)
        and Interrupt_Enable_Mask;
   end Pending_Interrupt_Mask;

   function Highest_Priority_Interrupt (Mask : Byte) return Interrupt_Type is
   begin
      for I in Interrupt_Type'Range loop
         if (Mask and Interrupt_Bits (I)) /= 0 then
            return I;
         end if;
      end loop;
      raise Program_Error with "no interrupt set in mask";
   end Highest_Priority_Interrupt;

   overriding
   procedure Reset (Interrupt_Flag : in out Interrupt_Flag_Type) is
   begin
      Interrupt_Flag.Map.Reg := 0;
   end Reset;

   overriding
   procedure Read
     (Interrupt_Flag : in out Interrupt_Flag_Type;
      GB             : in out Gade.GB.GB_Type;
      Address        : Word;
      Value          : out Byte)
   is
      pragma Unreferenced (GB, Address);
   begin
      Value := Interrupt_Flag.Map.Reg or Interrupt_Register_Read_Mask;
   end Read;

   overriding
   procedure Write
     (Interrupt_Flag : in out Interrupt_Flag_Type;
      GB             : in out Gade.GB.GB_Type;
      Address        : Word;
      Value          : Byte)
   is
      pragma Unreferenced (GB, Address);
   begin
      Interrupt_Flag.Map.Reg := Value and Interrupt_Enable_Mask;
   end Write;

   overriding
   procedure Reset (Interrupt_Enable : in out Interrupt_Enable_Type) is
   begin
      Interrupt_Enable.Map.Reg := 0;
   end Reset;

   overriding
   procedure Read
     (Interrupt_Enable : in out Interrupt_Enable_Type;
      GB               : in out Gade.GB.GB_Type;
      Address          : Word;
      Value            : out Byte)
   is
      pragma Unreferenced (GB, Address);
   begin
      Value := Interrupt_Enable.Map.Reg or Interrupt_Register_Read_Mask;
   end Read;

   overriding
   procedure Write
     (Interrupt_Enable : in out Interrupt_Enable_Type;
      GB               : in out Gade.GB.GB_Type;
      Address          : Word;
      Value            : Byte)
   is
      pragma Unreferenced (GB, Address);
   begin
      Interrupt_Enable.Map.Reg := Value and Interrupt_Enable_Mask;
   end Write;

   procedure Set_Interrupt (GB : in out Gade.GB.GB_Type; Interrupt : Interrupt_Type) is
   begin
      GB.Interrupt_Flag.Map.Flags (Interrupt) := True;
      if GB.Interrupt_Enable.Map.Flags (Interrupt) then
         --  TODO: Handle CPU stopped with Joypad interrupt
         GB.CPU.Halted := False;
      end if;
   end Set_Interrupt;

   function Interrupt_Requested
     (Interrupt_Flag : Interrupt_Flag_Register_Type) return Boolean is
   begin
      return Interrupt_Flag.Reg /= 0;
   end Interrupt_Requested;

   procedure Service_Interrupts (GB : in out Gade.GB.GB_Type; Cycles : out M_Cycle_Count)
   is
      Initial_Pending : Byte;
      Final_Pending   : Byte;
      Interrupt       : Interrupt_Type;
   begin
      if DMA_Active (GB.Display) then
         Cycles := 0;
         return;
      end if;

      if GB.CPU.IFF /= IE_EI then
         Cycles := 0;
         return;
      end if;

      Initial_Pending := Pending_Interrupt_Mask (GB);
      if Initial_Pending = 0 then
         Cycles := 0;
         return;
      end if;

      GB.CPU.Halted := False;
      GB.CPU.IFF := IE_DI;

      Gade.Dev.CPU.Instructions.Internal_Cycle (GB);
      Gade.Dev.CPU.Instructions.Internal_Cycle (GB);

      GB.CPU.Regs.SP := GB.CPU.Regs.SP - 1;
      Gade.Dev.CPU.Instructions.Bus_Write_Byte
        (GB, GB.CPU.Regs.SP, Byte (GB.CPU.PC / 2**8));

      GB.CPU.Regs.SP := GB.CPU.Regs.SP - 1;
      Gade.Dev.CPU.Instructions.Bus_Write_Byte
        (GB, GB.CPU.Regs.SP, Byte (GB.CPU.PC and 16#00FF#));

      Final_Pending := Pending_Interrupt_Mask (GB);

      if Final_Pending = 0 then
         GB.CPU.PC := 16#0000#;
      else
         Interrupt := Highest_Priority_Interrupt (Final_Pending);
         GB.Interrupt_Flag.Map.Flags (Interrupt) := False;
         GB.CPU.PC := Interrupt_Handlers (Interrupt);
      end if;

      Cycles := 4;
   end Service_Interrupts;

end Gade.Dev.Interrupts;
