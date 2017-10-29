package Gade.Dev.CPU is

   type CPU_Flag is private;

   type CPU_Flags is record
      C : CPU_Flag;
      --  Carry Flag
      N : CPU_Flag;
      --  Add/Subtract
      H : CPU_Flag;
      --  Half_Carry
      Z : CPU_Flag;
      --  Zero Flag
   end record;

   --------------------
   -- 15..8 --  7..0 --
   --------------------
   --   A   --   F   --
   --   B   --   C   --
   --   D   --   E   --
   --   H   --   L   --
   --       SP       --
   --       PC       --
   --------------------

   type Register_Width is (Half, Full);
   type CPU_Registers (Width : Register_Width := Half) is record
      SP : Word;
      case Width is
         when Half =>
            F : CPU_Flags;
            A, C, B, E, D, L, H : Byte;
         when Full =>
            AF, BC, DE, HL : Word;
      end case;
   end record with Unchecked_Union;

   type Interrupt_Enable is (IE_DI, IE_EI);

   type CPU_Context is tagged record
      Regs  : CPU_Registers;
      --  Flags : CPU_Flags;
      PC    : Word;
      IFF   : Interrupt_Enable; -- Interrupt Flipflops
      Halted : Boolean;
      --  Mem   : Memory_Map_Type;
      Branch_Taken : Boolean;
   end record;

   type P_CPU_Context is access CPU_Context;

   procedure Reset (ctxt : in out CPU_Context);

   procedure Set (Flag : in out CPU_Flag);
   pragma Inline (Set);

   procedure Set_Value (Flag : in out CPU_Flag; Value : in Boolean);
   pragma Inline (Set_Value);

   procedure Reset (Flag : in out CPU_Flag);
   pragma Inline (Reset);

   function Is_Set (Flag : CPU_Flag) return Boolean;
   pragma Inline (Is_Set);

   type Condition_Type is (C_Z, C_NZ, C_C, C_NC);

   function Check_Condition
      (CPU  : CPU_Context;
       cond : Condition_Type) return Boolean;

   function Get_Flags_String (CPU : CPU_Context) return String;

private

   type CPU_Flag is new Boolean;

   for CPU_Flags use record
      Z at 0 range 7 .. 7;
      N at 0 range 6 .. 6;
      H at 0 range 5 .. 5;
      C at 0 range 4 .. 4;
   end record;
   for CPU_Flags'Size use Byte'Size;

   for CPU_Registers use record
   end record;

end Gade.Dev.CPU;
