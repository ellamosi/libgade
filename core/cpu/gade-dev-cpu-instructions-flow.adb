with Gade.Dev.CPU.Instructions.Arithmetic;

package body Gade.Dev.CPU.Instructions.Flow is

   function Check_Condition
     (CPU       : CPU_Context;
      Condition : Instructions.Jump_Condition_Kind) return Boolean is
   begin
      case Condition is
         when Instructions.JCOND_None =>
            return True;
         when Instructions.JCOND_NZ =>
            return Gade.Dev.CPU.Check_Condition (CPU, C_NZ);
         when Instructions.JCOND_Z =>
            return Gade.Dev.CPU.Check_Condition (CPU, C_Z);
         when Instructions.JCOND_NC =>
            return Gade.Dev.CPU.Check_Condition (CPU, C_NC);
         when Instructions.JCOND_C =>
            return Gade.Dev.CPU.Check_Condition (CPU, C_C);
      end case;
   end Check_Condition;

   procedure Flow_Op
     (GB : in out Gade.GB.GB_Type) is
      Destination : Word;
      Offset      : Byte;
   begin
      case Operation is
         when Instructions.FLOW_RET =>
            if Check_Condition (GB.CPU, Condition) then
               GB.CPU.Branch_Taken := Condition /= Instructions.JCOND_None;
               Instructions.Internal_Cycle (GB);
               Instructions.Pop_Word (GB, GB.CPU.PC);
               if Condition /= Instructions.JCOND_None then
                  Instructions.Internal_Cycle (GB);
               end if;
            elsif Condition /= Instructions.JCOND_None then
               Instructions.Internal_Cycle (GB);
            end if;

         when Instructions.FLOW_RETI =>
            Instructions.Internal_Cycle (GB);
            Instructions.Pop_Word (GB, GB.CPU.PC);
            GB.CPU.IFF := IE_EI;

         when Instructions.FLOW_JR =>
            Offset := Instructions.Fetch_Imm8 (GB);
            if Check_Condition (GB.CPU, Condition) then
               GB.CPU.Branch_Taken := Condition /= Instructions.JCOND_None;
               Instructions.Internal_Cycle (GB);
               Gade.Dev.CPU.Instructions.Arithmetic.Add_Offset
                 (GB.CPU, GB.CPU.PC, Offset, False);
            end if;

         when Instructions.FLOW_JP =>
            if Target = Instructions.JTARGET_HL then
               if Check_Condition (GB.CPU, Condition) then
                  GB.CPU.PC := GB.CPU.Regs.HL;
               end if;
            else
               Destination := Instructions.Fetch_Imm16 (GB);
               if Check_Condition (GB.CPU, Condition) then
                  GB.CPU.Branch_Taken := Condition /= Instructions.JCOND_None;
                  Instructions.Internal_Cycle (GB);
                  GB.CPU.PC := Destination;
               end if;
            end if;

         when Instructions.FLOW_CALL =>
            Destination := Instructions.Fetch_Imm16 (GB);
            if Check_Condition (GB.CPU, Condition) then
               GB.CPU.Branch_Taken := Condition /= Instructions.JCOND_None;
               Instructions.Internal_Cycle (GB);
               Instructions.Push_Word (GB, GB.CPU.PC);
               GB.CPU.PC := Destination;
            end if;

         when Instructions.FLOW_RST =>
            Instructions.Internal_Cycle (GB);
            Instructions.Push_Word (GB, GB.CPU.PC);
            GB.CPU.PC := Vector;
      end case;
   end Flow_Op;

end Gade.Dev.CPU.Instructions.Flow;
