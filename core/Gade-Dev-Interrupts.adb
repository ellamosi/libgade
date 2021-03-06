with Gade.Dev.CPU;       use Gade.Dev.CPU;
with Gade.GB;            use Gade.GB;
with Gade.GB.Memory_Map; use Gade.GB.Memory_Map;

package body Gade.Dev.Interrupts is

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
      Value          : out Byte) is
      pragma Unreferenced (GB, Address);
   begin
      Value := Interrupt_Flag.Map.Reg;
   end Read;

   overriding
   procedure Write
     (Interrupt_Flag : in out Interrupt_Flag_Type;
      GB             : in out Gade.GB.GB_Type;
      Address        : Word;
      Value          : Byte) is
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
      Value            : out Byte) is
      pragma Unreferenced (GB, Address);
   begin
      Value := Interrupt_Enable.Map.Reg;
   end Read;

   overriding
   procedure Write
     (Interrupt_Enable : in out Interrupt_Enable_Type;
      GB               : in out Gade.GB.GB_Type;
      Address          : Word;
      Value            : Byte) is
      pragma Unreferenced (GB, Address);
   begin
      Interrupt_Enable.Map.Reg := Value and Interrupt_Enable_Mask;
   end Write;

   procedure Set_Interrupt
     (GB        : in out Gade.GB.GB_Type;
      Interrupt : Interrupt_Type) is
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

   procedure Service_Interrupts (GB     : in out Gade.GB.GB_Type;
                                 Cycles : out Natural) is
      Interrupt_Enable : Interrupt_Flag_Register_Type;
      Interrupt_Flags : Interrupt_Flag_Register_Type;
   begin
      if GB.CPU.IFF = IE_EI then
         Interrupt_Enable := GB.Interrupt_Enable.Map;
         Interrupt_Flags := GB.Interrupt_Flag.Map;
         for I in Interrupt_Type'Range loop
            --  Only the interrupt with highest priority gets attended
            if Interrupt_Flags.Flags (I) and Interrupt_Enable.Flags (I) then
               --  Reset interrupt flag
               Interrupt_Flags.Flags (I) := False;
               --  Disable further interrupts
               GB.CPU.IFF := IE_DI;
               --  Save current PC into the stack
               Push (GB, GB.CPU.PC);
               --  Jump to handler
               GB.CPU.PC := Interrupt_Handlers (I);
               --  Save reset interrupt flags
               GB.Interrupt_Flag.Map := Interrupt_Flags;
               --  Set interrupt servicing delay
               Cycles := 20;
               return;
            end if;
         end loop;
      end if;
      Cycles := 0;
   end Service_Interrupts;

end Gade.Dev.Interrupts;
