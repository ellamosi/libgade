package body Gade.Dev.CPU.Timing is

   function Is_Direct_Memory_Operand (Operand : Operand_Kind) return Boolean;

   function Is_HL_Memory_Operand (Operand : Operand_Kind) return Boolean;

   function Is_Register16_Operand (Operand : Operand_Kind) return Boolean;

   function Is_Direct_Memory_Operand (Operand : Operand_Kind) return Boolean is
   begin
      case Operand is
         when OD_Addr_BC | OD_Addr_DE | OD_Addr_HL |
              OD_Addr_HL_Inc | OD_Addr_HL_Dec |
              OD_Addr_Imm16 | OD_High_Addr_C | OD_High_Addr_Imm8 =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Direct_Memory_Operand;

   function Is_HL_Memory_Operand (Operand : Operand_Kind) return Boolean is
   begin
      case Operand is
         when OD_Addr_HL | OD_Addr_HL_Inc | OD_Addr_HL_Dec =>
            return True;
         when others =>
            return False;
      end case;
   end Is_HL_Memory_Operand;

   function Is_Register16_Operand (Operand : Operand_Kind) return Boolean is
   begin
      case Operand is
         when OD_AF | OD_BC | OD_DE | OD_HL | OD_SP | OD_PC =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Register16_Operand;

   function Cycles
     (Inst         : Decoded_Instruction;
      Branch_Taken : Boolean) return M_Cycle_Count is
   begin
      case Inst.Operation is
         when OP_Invalid =>
            raise Program_Error;

         when OP_NOP | OP_RLCA | OP_RRCA | OP_RLA | OP_RRA |
              OP_DAA | OP_CPL | OP_CCF | OP_SCF |
              OP_DI | OP_EI | OP_HALT | OP_STOP =>
            return 1;

         when OP_LD =>
            if Inst.Dest = OD_Addr_Imm16 and then Inst.Src = OD_SP then
               return 5;
            elsif Inst.Dest = OD_HL and then Inst.Src = OD_SP_Plus_Rel8 then
               return 3;
            elsif Inst.Dest = OD_SP and then Inst.Src = OD_HL then
               return 2;
            elsif Is_Register16_Operand (Inst.Dest) and then Inst.Src = OD_Imm16 then
               return 3;
            elsif Inst.Dest = OD_Addr_HL and then Inst.Src = OD_Imm8 then
               return 3;
            elsif Inst.Dest = OD_High_Addr_Imm8 or else Inst.Src = OD_High_Addr_Imm8 then
               return 3;
            elsif Inst.Dest = OD_Addr_Imm16 or else Inst.Src = OD_Addr_Imm16 then
               return 4;
            elsif Is_Direct_Memory_Operand (Inst.Dest) or else
                  Is_Direct_Memory_Operand (Inst.Src)
            then
               return 2;
            elsif Inst.Src = OD_Imm8 then
               return 2;
            else
               return 1;
            end if;

         when OP_PUSH =>
            return 4;

         when OP_POP =>
            return 3;

         when OP_ADD =>
            if Inst.Dest = OD_HL and then Is_Register16_Operand (Inst.Src) then
               return 2;
            elsif Inst.Dest = OD_SP and then Inst.Src = OD_Rel8 then
               return 4;
            elsif Inst.Src = OD_Imm8 or else Is_HL_Memory_Operand (Inst.Src) then
               return 2;
            else
               return 1;
            end if;

         when OP_ADC | OP_SUB | OP_SBC | OP_AND | OP_XOR | OP_OR | OP_CP =>
            if Inst.Src = OD_Imm8 or else Is_HL_Memory_Operand (Inst.Src) then
               return 2;
            else
               return 1;
            end if;

         when OP_INC | OP_DEC =>
            if Is_Register16_Operand (Inst.Dest) then
               return 2;
            elsif Is_HL_Memory_Operand (Inst.Dest) then
               return 3;
            else
               return 1;
            end if;

         when OP_JR =>
            if Inst.Condition = COND_None or else Branch_Taken then
               return 3;
            else
               return 2;
            end if;

         when OP_JP =>
            if Inst.Src = OD_HL then
               return 1;
            elsif Inst.Condition = COND_None or else Branch_Taken then
               return 4;
            else
               return 3;
            end if;

         when OP_CALL =>
            if Inst.Condition = COND_None or else Branch_Taken then
               return 6;
            else
               return 3;
            end if;

         when OP_RET =>
            if Inst.Condition = COND_None then
               return 4;
            elsif Branch_Taken then
               return 5;
            else
               return 2;
            end if;

         when OP_RETI =>
            return 4;

         when OP_RST =>
            return 4;

         when OP_BIT =>
            if Is_HL_Memory_Operand (Inst.Src) then
               return 3;
            else
               return 2;
            end if;

         when OP_SET | OP_RES |
              OP_RLC | OP_RRC | OP_RL | OP_RR |
              OP_SLA | OP_SRA | OP_SWAP | OP_SRL =>
            if Is_HL_Memory_Operand (Inst.Dest) then
               return 4;
            else
               return 2;
            end if;
      end case;
   end Cycles;

end Gade.Dev.CPU.Timing;
