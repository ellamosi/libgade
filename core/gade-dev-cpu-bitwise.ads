package Gade.Dev.CPU.Bitwise is

   --  Rotate and Shift Group

   procedure Do_RLC
      (CPU          : in out CPU_Context;
       Adjust_Flags : Boolean;
       Value        : in out Byte);
   --  The contents of the Accumulator (register A) are rotated left 1-bit
   --  position. The sign bit (bit 7) is copied to the Carry flag and also to
   --  bit 0. Bit 0 is the least-significant bit.
   --
   --  RLCA / RLC r / RLC (HL) / RLC (X+d) / RLC (IY+d)

   procedure Do_RL
      (CPU          : in out CPU_Context;
       Adjust_Flags : Boolean;
       Value        : in out Byte);

   procedure Do_RRC
      (CPU          : in out CPU_Context;
       Adjust_Flags : Boolean;
       Value        : in out Byte);

   procedure Do_RR
     (CPU           : in out CPU_Context;
       Adjust_Flags : Boolean;
      Value         : in out Byte);

   subtype Bit_Index is Natural range 0 .. 7;

   SR_SET : constant Bit := 1;
   SR_RES : constant Bit := 0;

   procedure Do_Set_Bit
      (CPU          : in out CPU_Context;
       Bit_Value    : Bit;
       Index        : Bit_Index;
       Value        : Byte;
       Result       : out Byte);

   procedure Do_Bit
      (CPU          : in out CPU_Context;
       Index        : Bit_Index;
       Value        : Byte);

   S_L : constant Boolean := False;
   S_A : constant Boolean := True;

   procedure Do_SL
      (CPU        : in out CPU_Context;
       Arithmetic : Boolean;
       Value      : in out Byte);

   procedure Do_SR
      (CPU        : in out CPU_Context;
       Arithmetic : Boolean;
       Value      : in out Byte);

   --  Swap upper & lower nibbles of n.
   procedure Do_Swap
      (CPU        : in out CPU_Context;
       Value      : in out Byte);

end Gade.Dev.CPU.Bitwise;
