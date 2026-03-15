with Gade.Dev.CPU.Arithmetic; use Gade.Dev.CPU.Arithmetic;
with Gade.Dev.CPU.Bitwise;    use Gade.Dev.CPU.Bitwise;
with Gade.Dev.CPU.Decode;     use Gade.Dev.CPU.Decode;
with Gade.Dev.CPU.Logic;      use Gade.Dev.CPU.Logic;
with Gade.GB;                 use Gade.GB;
with Gade.GB.Memory_Map;      use Gade.GB.Memory_Map;

package body Gade.Dev.CPU.Decoded_Instructions is

   function Check_Inst_Condition
     (CPU       : CPU_Context;
      Condition : Condition_Kind) return Boolean;

   function Is_Byte_Operand
     (Operand : Operand_Kind) return Boolean;

   function Is_Word_Operand
     (Operand : Operand_Kind) return Boolean;

   procedure Adjust_HL_Auto
     (GB      : in out GB_Type;
      Operand :        Operand_Kind);

   function Read_Byte_Operand
     (GB      : in out GB_Type;
      Inst    :        Decoded_Instruction;
      Operand :        Operand_Kind) return Byte;

   function Read_Word_Operand
     (GB      : in out GB_Type;
      Inst    :        Decoded_Instruction;
      Operand :        Operand_Kind) return Word;

   procedure Write_Byte_Operand
     (GB      : in out GB_Type;
      Operand :        Operand_Kind;
      Value   :        Byte);

   procedure Write_Word_Operand
     (GB      : in out GB_Type;
      Operand :        Operand_Kind;
      Value   :        Word);

   procedure Execute_LD
     (GB   : in out GB_Type;
      Inst :        Decoded_Instruction);

   procedure Execute_ADD
     (GB   : in out GB_Type;
      Inst :        Decoded_Instruction);

   procedure Execute_Rotate_Shift
     (GB   : in out GB_Type;
      Inst :        Decoded_Instruction);

   function Check_Inst_Condition
     (CPU       : CPU_Context;
      Condition : Condition_Kind) return Boolean is
   begin
      case Condition is
         when COND_None =>
            return True;
         when COND_NZ =>
            return Check_Condition (CPU, C_NZ);
         when COND_Z =>
            return Check_Condition (CPU, C_Z);
         when COND_NC =>
            return Check_Condition (CPU, C_NC);
         when COND_C =>
            return Check_Condition (CPU, C_C);
      end case;
   end Check_Inst_Condition;

   function Is_Byte_Operand
     (Operand : Operand_Kind) return Boolean is
   begin
      case Operand is
         when OD_A | OD_B | OD_C | OD_D | OD_E | OD_H | OD_L |
              OD_Addr_BC | OD_Addr_DE | OD_Addr_HL |
              OD_Addr_HL_Inc | OD_Addr_HL_Dec | OD_Addr_Imm16 |
              OD_High_Addr_C | OD_High_Addr_Imm8 |
              OD_Imm8 =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Byte_Operand;

   function Is_Word_Operand
     (Operand : Operand_Kind) return Boolean is
   begin
      case Operand is
         when OD_AF | OD_BC | OD_DE | OD_HL | OD_SP | OD_PC |
              OD_Imm16 | OD_SP_Plus_Rel8 | OD_RST_Vector =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Word_Operand;

   procedure Adjust_HL_Auto
     (GB      : in out GB_Type;
      Operand :        Operand_Kind) is
   begin
      case Operand is
         when OD_Addr_HL_Inc =>
            GB.CPU.Regs.HL := GB.CPU.Regs.HL + 1;
         when OD_Addr_HL_Dec =>
            GB.CPU.Regs.HL := GB.CPU.Regs.HL - 1;
         when others =>
            null;
      end case;
   end Adjust_HL_Auto;

   function Read_Byte_Operand
     (GB      : in out GB_Type;
      Inst    :        Decoded_Instruction;
      Operand :        Operand_Kind) return Byte is
   begin
      case Operand is
         when OD_A =>
            return GB.CPU.Regs.A;
         when OD_B =>
            return GB.CPU.Regs.B;
         when OD_C =>
            return GB.CPU.Regs.C;
         when OD_D =>
            return GB.CPU.Regs.D;
         when OD_E =>
            return GB.CPU.Regs.E;
         when OD_H =>
            return GB.CPU.Regs.H;
         when OD_L =>
            return GB.CPU.Regs.L;
         when OD_Addr_BC =>
            return Read_Byte (GB, GB.CPU.Regs.BC);
         when OD_Addr_DE =>
            return Read_Byte (GB, GB.CPU.Regs.DE);
         when OD_Addr_HL =>
            return Read_Byte (GB, GB.CPU.Regs.HL);
         when OD_Addr_HL_Inc | OD_Addr_HL_Dec =>
            return Read_Byte (GB, GB.CPU.Regs.HL);
         when OD_Addr_Imm16 =>
            return Read_Byte (GB, Inst.Imm16);
         when OD_High_Addr_C =>
            return Read_Byte (GB, 16#FF00# + Word (GB.CPU.Regs.C));
         when OD_High_Addr_Imm8 =>
            return Read_Byte (GB, 16#FF00# + Word (Inst.Imm8));
         when OD_Imm8 =>
            return Inst.Imm8;
         when others =>
            raise Program_Error;
      end case;
   end Read_Byte_Operand;

   function Read_Word_Operand
     (GB      : in out GB_Type;
      Inst    :        Decoded_Instruction;
      Operand :        Operand_Kind) return Word is
      Result : Word;
   begin
      case Operand is
         when OD_AF =>
            return GB.CPU.Regs.AF;
         when OD_BC =>
            return GB.CPU.Regs.BC;
         when OD_DE =>
            return GB.CPU.Regs.DE;
         when OD_HL =>
            return GB.CPU.Regs.HL;
         when OD_SP =>
            return GB.CPU.Regs.SP;
         when OD_PC =>
            return GB.CPU.PC;
         when OD_Imm16 =>
            return Inst.Imm16;
         when OD_RST_Vector =>
            return Inst.RST_Vector;
         when OD_SP_Plus_Rel8 =>
            Result := GB.CPU.Regs.SP;
            Add_Offset (GB.CPU, Result, Inst.Imm8, True);
            return Result;
         when others =>
            raise Program_Error;
      end case;
   end Read_Word_Operand;

   procedure Write_Byte_Operand
     (GB      : in out GB_Type;
      Operand :        Operand_Kind;
      Value   :        Byte) is
   begin
      case Operand is
         when OD_A =>
            GB.CPU.Regs.A := Value;
         when OD_B =>
            GB.CPU.Regs.B := Value;
         when OD_C =>
            GB.CPU.Regs.C := Value;
         when OD_D =>
            GB.CPU.Regs.D := Value;
         when OD_E =>
            GB.CPU.Regs.E := Value;
         when OD_H =>
            GB.CPU.Regs.H := Value;
         when OD_L =>
            GB.CPU.Regs.L := Value;
         when OD_Addr_BC =>
            Write_Byte (GB, GB.CPU.Regs.BC, Value);
         when OD_Addr_DE =>
            Write_Byte (GB, GB.CPU.Regs.DE, Value);
         when OD_Addr_HL | OD_Addr_HL_Inc | OD_Addr_HL_Dec =>
            Write_Byte (GB, GB.CPU.Regs.HL, Value);
         when OD_Addr_Imm16 =>
            Write_Byte (GB, Read_Word_Operand (GB, (others => <>), OD_Imm16), Value);
         when OD_High_Addr_C =>
            Write_Byte (GB, 16#FF00# + Word (GB.CPU.Regs.C), Value);
         when OD_High_Addr_Imm8 =>
            raise Program_Error;
         when others =>
            raise Program_Error;
      end case;
   end Write_Byte_Operand;

   procedure Write_Word_Operand
     (GB      : in out GB_Type;
      Operand :        Operand_Kind;
      Value   :        Word) is
   begin
      case Operand is
         when OD_AF =>
            GB.CPU.Regs.AF := Value and 16#FFF0#;
         when OD_BC =>
            GB.CPU.Regs.BC := Value;
         when OD_DE =>
            GB.CPU.Regs.DE := Value;
         when OD_HL =>
            GB.CPU.Regs.HL := Value;
         when OD_SP =>
            GB.CPU.Regs.SP := Value;
         when OD_PC =>
            GB.CPU.PC := Value;
         when OD_Addr_Imm16 =>
            Write_Word (GB, Value, GB.CPU.Regs.SP);
         when others =>
            raise Program_Error;
      end case;
   end Write_Word_Operand;

   procedure Execute_LD
     (GB   : in out GB_Type;
      Inst :        Decoded_Instruction) is
      Byte_Value : Byte;
      Word_Value : Word;
   begin
      if Inst.Src = OD_SP_Plus_Rel8 then
         Word_Value := Read_Word_Operand (GB, Inst, Inst.Src);
         Write_Word_Operand (GB, Inst.Dest, Word_Value);
      elsif Inst.Dest = OD_Addr_Imm16 and then Inst.Src = OD_SP then
         Write_Word (GB, Inst.Imm16, GB.CPU.Regs.SP);
      elsif Is_Word_Operand (Inst.Dest) and then Is_Word_Operand (Inst.Src) then
         Word_Value := Read_Word_Operand (GB, Inst, Inst.Src);
         Write_Word_Operand (GB, Inst.Dest, Word_Value);
      elsif Is_Byte_Operand (Inst.Dest) and then Is_Byte_Operand (Inst.Src) then
         Byte_Value := Read_Byte_Operand (GB, Inst, Inst.Src);
         case Inst.Dest is
            when OD_High_Addr_Imm8 =>
               Write_Byte (GB, 16#FF00# + Word (Inst.Imm8), Byte_Value);
            when OD_Addr_Imm16 =>
               Write_Byte (GB, Inst.Imm16, Byte_Value);
            when others =>
               Write_Byte_Operand (GB, Inst.Dest, Byte_Value);
         end case;
      else
         raise Program_Error;
      end if;

      Adjust_HL_Auto (GB, Inst.Src);
      Adjust_HL_Auto (GB, Inst.Dest);
   end Execute_LD;

   procedure Execute_ADD
     (GB   : in out GB_Type;
      Inst :        Decoded_Instruction) is
   begin
      if Inst.Dest = OD_A then
         Do_Add (GB.CPU, Read_Byte_Operand (GB, Inst, Inst.Src), GB.CPU.Regs.A, ADD_Carry);
      elsif Inst.Dest = OD_HL then
         Do_Add (GB.CPU, Read_Word_Operand (GB, Inst, Inst.Src), GB.CPU.Regs.HL);
      elsif Inst.Dest = OD_SP and then Inst.Src = OD_Rel8 then
         Do_Add (GB.CPU, GB.CPU.Regs.SP, Inst.Imm8);
      else
         raise Program_Error;
      end if;
   end Execute_ADD;

   procedure Execute_Rotate_Shift
     (GB   : in out GB_Type;
      Inst :        Decoded_Instruction) is
      Value        : Byte;
      Adjust_Flags : constant Boolean :=
        Inst.Operation not in OP_RLCA | OP_RRCA | OP_RLA | OP_RRA;
      Target       : constant Operand_Kind :=
        (case Inst.Operation is
            when OP_RLCA | OP_RRCA | OP_RLA | OP_RRA => OD_A,
            when others => Inst.Dest);
   begin
      Value := Read_Byte_Operand (GB, Inst, Target);

      case Inst.Operation is
         when OP_RLC | OP_RLCA =>
            Do_RLC (GB.CPU, Adjust_Flags, Value);
         when OP_RRC | OP_RRCA =>
            Do_RRC (GB.CPU, Adjust_Flags, Value);
         when OP_RL | OP_RLA =>
            Do_RL (GB.CPU, Adjust_Flags, Value);
         when OP_RR | OP_RRA =>
            Do_RR (GB.CPU, Adjust_Flags, Value);
         when OP_SLA =>
            Do_SL (GB.CPU, S_A, Value);
         when OP_SRA =>
            Do_SR (GB.CPU, S_A, Value);
         when OP_SWAP =>
            Do_Swap (GB.CPU, Value);
         when OP_SRL =>
            Do_SR (GB.CPU, S_L, Value);
         when others =>
            raise Program_Error;
      end case;

      Write_Byte_Operand (GB, Target, Value);
   end Execute_Rotate_Shift;

   procedure Execute
     (GB   : in out GB_Type;
      Inst :        Decoded_Instruction) is
      Byte_Value : Byte;
      Word_Value : Word;
      Dummy      : Byte;
   begin
      case Inst.Operation is
         when OP_Invalid =>
            raise Program_Error;

         when OP_NOP =>
            null;

         when OP_LD =>
            Execute_LD (GB, Inst);

         when OP_PUSH =>
            Push (GB, Read_Word_Operand (GB, Inst, Inst.Src));

         when OP_POP =>
            Pop (GB, Word_Value);
            Write_Word_Operand (GB, Inst.Dest, Word_Value);

         when OP_ADD =>
            Execute_ADD (GB, Inst);

         when OP_ADC =>
            Do_Add (GB.CPU, Read_Byte_Operand (GB, Inst, Inst.Src), GB.CPU.Regs.A, ADC_Carry);

         when OP_SUB =>
            Do_Sub (GB.CPU, Read_Byte_Operand (GB, Inst, Inst.Src), GB.CPU.Regs.A, SUB_Carry);

         when OP_SBC =>
            Do_Sub (GB.CPU, Read_Byte_Operand (GB, Inst, Inst.Src), GB.CPU.Regs.A, SBC_Carry);

         when OP_AND =>
            Do_AND (GB.CPU, Read_Byte_Operand (GB, Inst, Inst.Src));

         when OP_XOR =>
            Do_XOR (GB.CPU, Read_Byte_Operand (GB, Inst, Inst.Src));

         when OP_OR =>
            Do_OR (GB.CPU, Read_Byte_Operand (GB, Inst, Inst.Src));

         when OP_CP =>
            Do_Sub (GB.CPU, Read_Byte_Operand (GB, Inst, Inst.Src), Dummy, SUB_Carry);

         when OP_INC =>
            if Is_Word_Operand (Inst.Dest) then
               Write_Word_Operand
                 (GB, Inst.Dest, Read_Word_Operand (GB, Inst, Inst.Dest) + 1);
            else
               Byte_Value := Read_Byte_Operand (GB, Inst, Inst.Dest);
               Do_Inc_Dec (GB.CPU, INC, Byte_Value, Byte_Value);
               Write_Byte_Operand (GB, Inst.Dest, Byte_Value);
            end if;

         when OP_DEC =>
            if Is_Word_Operand (Inst.Dest) then
               Write_Word_Operand
                 (GB, Inst.Dest, Read_Word_Operand (GB, Inst, Inst.Dest) - 1);
            else
               Byte_Value := Read_Byte_Operand (GB, Inst, Inst.Dest);
               Do_Inc_Dec (GB.CPU, DEC, Byte_Value, Byte_Value);
               Write_Byte_Operand (GB, Inst.Dest, Byte_Value);
            end if;

         when OP_DAA =>
            Do_Daa (GB.CPU);

         when OP_CPL =>
            GB.CPU.Regs.A := not GB.CPU.Regs.A;
            Set (GB.CPU.Regs.F.H);
            Set (GB.CPU.Regs.F.N);

         when OP_CCF =>
            Reset (GB.CPU.Regs.F.N);
            Reset (GB.CPU.Regs.F.H);
            Set_Value (GB.CPU.Regs.F.C, not Is_Set (GB.CPU.Regs.F.C));

         when OP_SCF =>
            Reset (GB.CPU.Regs.F.N);
            Reset (GB.CPU.Regs.F.H);
            Set (GB.CPU.Regs.F.C);

         when OP_JR =>
            if Check_Inst_Condition (GB.CPU, Inst.Condition) then
               GB.CPU.Branch_Taken := Inst.Condition /= COND_None;
               Add_Offset (GB.CPU, GB.CPU.PC, Inst.Imm8, False);
            end if;

         when OP_JP =>
            if Check_Inst_Condition (GB.CPU, Inst.Condition) then
               GB.CPU.Branch_Taken := Inst.Condition /= COND_None;
               GB.CPU.PC := Read_Word_Operand (GB, Inst, Inst.Src);
            end if;

         when OP_CALL =>
            if Check_Inst_Condition (GB.CPU, Inst.Condition) then
               GB.CPU.Branch_Taken := Inst.Condition /= COND_None;
               Push (GB, GB.CPU.PC);
               GB.CPU.PC := Read_Word_Operand (GB, Inst, Inst.Src);
            end if;

         when OP_RET =>
            if Check_Inst_Condition (GB.CPU, Inst.Condition) then
               GB.CPU.Branch_Taken := Inst.Condition /= COND_None;
               Pop (GB, GB.CPU.PC);
            end if;

         when OP_RETI =>
            Pop (GB, GB.CPU.PC);
            GB.CPU.IFF := IE_EI;

         when OP_RST =>
            Push (GB, GB.CPU.PC);
            GB.CPU.PC := Inst.RST_Vector;

         when OP_BIT =>
            Do_Bit (GB.CPU, Bit_Index (Inst.Bit_Index), Read_Byte_Operand (GB, Inst, Inst.Src));

         when OP_SET =>
            Byte_Value := Read_Byte_Operand (GB, Inst, Inst.Dest);
            Do_Set_Bit (GB.CPU, SR_SET, Bit_Index (Inst.Bit_Index), Byte_Value, Byte_Value);
            Write_Byte_Operand (GB, Inst.Dest, Byte_Value);

         when OP_RES =>
            Byte_Value := Read_Byte_Operand (GB, Inst, Inst.Dest);
            Do_Set_Bit (GB.CPU, SR_RES, Bit_Index (Inst.Bit_Index), Byte_Value, Byte_Value);
            Write_Byte_Operand (GB, Inst.Dest, Byte_Value);

         when OP_RLC | OP_RRC | OP_RL | OP_RR |
              OP_RLCA | OP_RRCA | OP_RLA | OP_RRA |
              OP_SLA | OP_SRA | OP_SWAP | OP_SRL =>
            Execute_Rotate_Shift (GB, Inst);

         when OP_DI =>
            GB.CPU.IFF := IE_DI;

         when OP_EI =>
            GB.CPU.IFF := IE_EI;

         when OP_HALT =>
            GB.CPU.Halted := True;

         when OP_STOP =>
            if GB.CPU.IFF = IE_EI then
               GB.CPU.Halted := True;
            end if;
      end case;
   end Execute;

end Gade.Dev.CPU.Decoded_Instructions;
