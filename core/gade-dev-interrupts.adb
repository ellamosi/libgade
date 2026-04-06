with Gade.Dev.CPU;     use Gade.Dev.CPU;
with Gade.Dev.CPU.Instructions;
with Gade.Dev.Display; use Gade.Dev.Display;
with Gade.GB;          use Gade.GB;

package body Gade.Dev.Interrupts is

   Interrupt_Register_Read_Mask : constant Byte := 16#E0#;

   function Pending_Interrupt_Mask (GB : Gade.GB.GB_Type) return Byte;

   function Interrupt_Set (Mask : Byte; Interrupt : Interrupt_Type) return Boolean;

   function Highest_Priority_Interrupt (Mask : Byte) return Interrupt_Type;

   function Pending_Interrupt_Mask (GB : Gade.GB.GB_Type) return Byte is
   begin
      return
        (GB.Interrupt_Enable.Map.Reg and GB.Interrupt_Flag.Map.Reg)
        and Interrupt_Enable_Mask;
   end Pending_Interrupt_Mask;

   function Has_Pending_Enabled_Interrupt (GB : Gade.GB.GB_Type) return Boolean is
   begin
      return Pending_Interrupt_Mask (GB) /= 0;
   end Has_Pending_Enabled_Interrupt;

   function Interrupt_Set (Mask : Byte; Interrupt : Interrupt_Type) return Boolean is
      Register : constant Interrupt_Flag_Register_Type :=
        (Access_Type => Address, Reg => Mask);
   begin
      return
        (case Interrupt is
           when VBlank_Interrupt => Register.VBLANK,
           when LCDC_Interrupt   => Register.LCDC,
           when Timer_Interrupt  => Register.Timer,
           when Serial_Interrupt => Register.Serial,
           when Joypad_Interrupt => Register.Joypad);
   end Interrupt_Set;

   function Highest_Priority_Interrupt (Mask : Byte) return Interrupt_Type is
   begin
      for I in Interrupt_Type'Range loop
         if Interrupt_Set (Mask, I) then
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
         GB.CPU.Execution_State := Running;
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
      Cycles := 0;

      --  Pan Docs recommends preventing interrupts during DMG OAM DMA; defer
      --  servicing here because interrupt entry needs non-HRAM memory accesses.
      if DMA_Active (GB.Display) then
         return;
      end if;

      if GB.CPU.IFF /= IME_Enabled then
         return;
      end if;

      Initial_Pending := Pending_Interrupt_Mask (GB);
      if Initial_Pending = 0 then
         return;
      end if;

      --  Accept the interrupt and disable further servicing until the handler
      --  explicitly re-enables IME.
      GB.CPU.Execution_State := Running;
      GB.CPU.IFF := IME_Disabled;

      --  Interrupt entry consumes two internal M-cycles before the stack
      --  writes.
      Gade.Dev.CPU.Instructions.Internal_Cycles (GB, 2);

      --  Save the current PC on the stack, high byte first, while keeping the
      --  device state ticking one M-cycle at a time.
      Gade.Dev.CPU.Instructions.Push_Word (GB, GB.CPU.PC);

      Final_Pending := Pending_Interrupt_Mask (GB);

      --  Interrupt dispatch canceling
      --  From: https://mgba-emu.github.io/gbdoc/#interrupt-dispatch-canceling

      --  Interrupt dispatch is not atomic. If the value of IE AND IF changes
      --  between the first M cycle and when the vector is loaded, the value
      --  of the vector may change. This can happen if IE or IF is altered in
      --  some way, either by a higher priority IRQ asserting, or by another
      --  memory write (e.g. the first stack push overwriting IE or IF).

      --  - If a higher priority IRQ is asserted, it effectively steals the
      --    interrupt dispatch
      --  - If IE AND IF becomes zero, no interrupt vector can be located and
      --    instead $0000 is loaded into the program counter.

      if Final_Pending = 0 then
         GB.CPU.PC := 16#0000#;
      else
         Interrupt := Highest_Priority_Interrupt (Final_Pending);
         --  Reset the serviced interrupt flag before entering its handler.
         GB.Interrupt_Flag.Map.Flags (Interrupt) := False;
         GB.CPU.PC := Interrupt_Handlers (Interrupt);
      end if;

      --  Report the interrupt service cost for Run_For accounting only; the
      --  per-M-cycle state advancement already happened above.
      Cycles := 4;
   end Service_Interrupts;

end Gade.Dev.Interrupts;
