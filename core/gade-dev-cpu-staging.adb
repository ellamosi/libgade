package body Gade.Dev.CPU.Staging is

   type Stage_Template_Table is array (Byte) of Stage_Template;

   function Is_Direct_Memory_Operand (Operand : Operand_Kind) return Boolean;

   function Is_HL_Memory_Operand (Operand : Operand_Kind) return Boolean;

   function Is_Register16_Operand (Operand : Operand_Kind) return Boolean;

   function Build_Stage_Table
     (Prefix : Prefix_Kind) return Stage_Template_Table;

   function Build_Template
     (Inst : Decoded_Instruction) return Stage_Template;

   procedure Append
     (Template : in out Stage_Template;
      Kind     :        Stage_Kind;
      Cost     :        M_Cycle_Count := 1;
      Flag     :        Stage_Flag := None);

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

   procedure Append
     (Template : in out Stage_Template;
      Kind     :        Stage_Kind;
      Cost     :        M_Cycle_Count := 1;
      Flag     :        Stage_Flag := None) is
   begin
      Template.Count := Template.Count + 1;
      Template.Items (Positive (Template.Count)) :=
        (Kind => Kind, Cost => Cost, Flag => Flag);
   end Append;

   function Build_Template
     (Inst : Decoded_Instruction) return Stage_Template
   is
      Template : Stage_Template := Empty_Template;
   begin
      case Inst.Operation is
         when OP_Invalid =>
            null;

         when OP_NOP | OP_RLCA | OP_RRCA | OP_RLA | OP_RRA |
              OP_DAA | OP_CPL | OP_CCF | OP_SCF |
              OP_DI | OP_EI | OP_HALT | OP_STOP =>
            Append (Template, Commit);

         when OP_LD =>
            if Inst.Dest = OD_Addr_Imm16 and then Inst.Src = OD_SP then
               Append (Template, Fetch_Imm16_Lo);
               Append (Template, Fetch_Imm16_Hi);
               Append (Template, Write);
               Append (Template, Write);
               Append (Template, Commit);
            elsif Inst.Dest = OD_HL and then Inst.Src = OD_SP_Plus_Rel8 then
               Append (Template, Fetch_Imm8);
               Append (Template, Internal);
               Append (Template, Commit);
            elsif Inst.Dest = OD_SP and then Inst.Src = OD_HL then
               Append (Template, Internal);
               Append (Template, Commit);
            elsif Is_Register16_Operand (Inst.Dest) and then Inst.Src = OD_Imm16 then
               Append (Template, Fetch_Imm16_Lo);
               Append (Template, Fetch_Imm16_Hi);
               Append (Template, Commit);
            elsif Inst.Dest = OD_Addr_HL and then Inst.Src = OD_Imm8 then
               Append (Template, Fetch_Imm8);
               Append (Template, Write);
               Append (Template, Commit);
            elsif Inst.Dest = OD_High_Addr_Imm8 or else Inst.Src = OD_High_Addr_Imm8 then
               Append (Template, Fetch_Imm8);
               Append
                 (Template,
                  (if Inst.Dest = OD_High_Addr_Imm8 then Write else Read));
               Append (Template, Commit);
            elsif Inst.Dest = OD_Addr_Imm16 or else Inst.Src = OD_Addr_Imm16 then
               Append (Template, Fetch_Imm16_Lo);
               Append (Template, Fetch_Imm16_Hi);
               Append
                 (Template,
                  (if Inst.Dest = OD_Addr_Imm16 then Write else Read));
               Append (Template, Commit);
            elsif Is_Direct_Memory_Operand (Inst.Dest) or else
                  Is_Direct_Memory_Operand (Inst.Src)
            then
               Append
                 (Template,
                  (if Is_Direct_Memory_Operand (Inst.Dest) then Write else Read));
               Append (Template, Commit);
            elsif Inst.Src = OD_Imm8 then
               Append (Template, Fetch_Imm8);
               Append (Template, Commit);
            else
               Append (Template, Commit);
            end if;

         when OP_PUSH =>
            Append (Template, Internal);
            Append (Template, Push_Hi);
            Append (Template, Push_Lo);
            Append (Template, Commit);

         when OP_POP =>
            Append (Template, Pop_Lo);
            Append (Template, Pop_Hi);
            Append (Template, Commit);

         when OP_ADD =>
            if Inst.Dest = OD_HL and then Is_Register16_Operand (Inst.Src) then
               Append (Template, Internal);
               Append (Template, Commit);
            elsif Inst.Dest = OD_SP and then Inst.Src = OD_Rel8 then
               Append (Template, Fetch_Imm8);
               Append (Template, Internal);
               Append (Template, Internal);
               Append (Template, Commit);
            elsif Inst.Src = OD_Imm8 then
               Append (Template, Fetch_Imm8);
               Append (Template, Commit);
            elsif Is_HL_Memory_Operand (Inst.Src) then
               Append (Template, Read);
               Append (Template, Commit);
            else
               Append (Template, Commit);
            end if;

         when OP_ADC | OP_SUB | OP_SBC | OP_AND | OP_XOR | OP_OR | OP_CP =>
            if Inst.Src = OD_Imm8 then
               Append (Template, Fetch_Imm8);
               Append (Template, Commit);
            elsif Is_HL_Memory_Operand (Inst.Src) then
               Append (Template, Read);
               Append (Template, Commit);
            else
               Append (Template, Commit);
            end if;

         when OP_INC | OP_DEC =>
            if Is_Register16_Operand (Inst.Dest) then
               Append (Template, Internal);
               Append (Template, Commit);
            elsif Is_HL_Memory_Operand (Inst.Dest) then
               Append (Template, Read);
               Append (Template, Write);
               Append (Template, Commit);
            else
               Append (Template, Commit);
            end if;

         when OP_JR =>
            Append (Template, Fetch_Imm8);
            if Inst.Condition /= COND_None then
               Append (Template, Cond_Check);
               Append (Template, Commit, Flag => Skip_If_Not_Taken);
            else
               Append (Template, Internal);
               Append (Template, Commit);
            end if;

         when OP_JP =>
            if Inst.Src = OD_HL then
               Append (Template, Commit);
            else
               Append (Template, Fetch_Imm16_Lo);
               Append (Template, Fetch_Imm16_Hi);
               if Inst.Condition /= COND_None then
                  Append (Template, Cond_Check);
                  Append (Template, Commit, Flag => Skip_If_Not_Taken);
               else
                  Append (Template, Internal);
                  Append (Template, Commit);
               end if;
            end if;

         when OP_CALL =>
            Append (Template, Fetch_Imm16_Lo);
            Append (Template, Fetch_Imm16_Hi);
            if Inst.Condition /= COND_None then
               Append (Template, Cond_Check);
               Append (Template, Push_Hi, Flag => Skip_If_Not_Taken);
               Append (Template, Push_Lo, Flag => Skip_If_Not_Taken);
               Append (Template, Commit, Flag => Skip_If_Not_Taken);
            else
               Append (Template, Internal);
               Append (Template, Push_Hi);
               Append (Template, Push_Lo);
               Append (Template, Commit);
            end if;

         when OP_RET =>
            if Inst.Condition /= COND_None then
               Append (Template, Cond_Check);
               Append (Template, Internal);
               Append (Template, Pop_Lo, Flag => Skip_If_Not_Taken);
               Append (Template, Pop_Hi, Flag => Skip_If_Not_Taken);
               Append (Template, Commit, Flag => Skip_If_Not_Taken);
            else
               Append (Template, Pop_Lo);
               Append (Template, Pop_Hi);
               Append (Template, Commit);
               Append (Template, Internal);
            end if;

         when OP_RETI =>
            Append (Template, Pop_Lo);
            Append (Template, Pop_Hi);
            Append (Template, Commit);
            Append (Template, Internal);

         when OP_RST =>
            Append (Template, Internal);
            Append (Template, Push_Hi);
            Append (Template, Push_Lo);
            Append (Template, Commit);

         when OP_BIT =>
            if Is_HL_Memory_Operand (Inst.Src) then
               Append (Template, Read);
               Append (Template, Internal);
               Append (Template, Commit);
            else
               Append (Template, Internal);
               Append (Template, Commit);
            end if;

         when OP_SET | OP_RES |
              OP_RLC | OP_RRC | OP_RL | OP_RR |
              OP_SLA | OP_SRA | OP_SWAP | OP_SRL =>
            if Is_HL_Memory_Operand (Inst.Dest) then
               Append (Template, Read);
               Append (Template, Internal);
               Append (Template, Write);
               Append (Template, Commit);
            else
               Append (Template, Internal);
               Append (Template, Commit);
            end if;
      end case;

      return Template;
   end Build_Template;

   function Build_Stage_Table
     (Prefix : Prefix_Kind) return Stage_Template_Table is
      Result : Stage_Template_Table := [others => Empty_Template];
   begin
      for Opcode in Byte'Range loop
         Result (Opcode) :=
           Build_Template (Decode_Template (Prefix => Prefix, Opcode => Opcode));
      end loop;

      return Result;
   end Build_Stage_Table;

   Main_Stage_Table : constant Stage_Template_Table := Build_Stage_Table (Main);
   CB_Stage_Table   : constant Stage_Template_Table := Build_Stage_Table (CB);

   function Template_For
     (Inst : Decoded_Instruction) return Stage_Template is
   begin
      case Inst.Prefix is
         when Main =>
            return Main_Stage_Table (Inst.Opcode);
         when CB =>
            return CB_Stage_Table (Inst.Opcode);
      end case;
   end Template_For;

end Gade.Dev.CPU.Staging;
