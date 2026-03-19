package Gade.Dev.CPU.Instructions.Bitwise is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure Do_RLC
     (CPU          : in out CPU_Context;
      Adjust_Flags :        Boolean;
      Value        : in out Byte);

   procedure Do_RL
     (CPU          : in out CPU_Context;
      Adjust_Flags :        Boolean;
      Value        : in out Byte);

   procedure Do_RRC
     (CPU          : in out CPU_Context;
      Adjust_Flags :        Boolean;
      Value        : in out Byte);

   procedure Do_RR
     (CPU          : in out CPU_Context;
      Adjust_Flags :        Boolean;
      Value        : in out Byte);

   SR_SET : constant Bit := 1;
   SR_RES : constant Bit := 0;

   procedure Do_Set_Bit
     (CPU       : in out CPU_Context;
      Bit_Value :        Bit;
      Index     :        Instructions.Bit_Index;
      Value     :        Byte;
      Result    :    out Byte);

   procedure Do_Bit
     (CPU   : in out CPU_Context;
      Index :        Instructions.Bit_Index;
      Value :        Byte);

   S_L : constant Boolean := False;
   S_A : constant Boolean := True;

   procedure Do_SL
     (CPU        : in out CPU_Context;
      Arithmetic :        Boolean;
      Value      : in out Byte);

   procedure Do_SR
     (CPU        : in out CPU_Context;
      Arithmetic :        Boolean;
      Value      : in out Byte);

   procedure Do_Swap
     (CPU   : in out CPU_Context;
      Value : in out Byte);

   generic
      Operation : Instructions.Bit_Op_Kind;
      Index     : Instructions.Bit_Index;
      Target    : Instructions.Byte_Source_Kind;
   procedure Execute_Bit_Source
     (GB : in out Gade.GB.GB_Type);

   generic
      Operation    : Instructions.Rotate_Shift_Op_Kind;
      Target       : Instructions.Byte_Target_Kind;
      Adjust_Flags : Boolean := True;
   procedure Execute_Rotate_Shift
     (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Instructions.Bitwise;
