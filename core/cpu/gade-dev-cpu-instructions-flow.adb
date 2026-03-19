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


   procedure Execute_Flow
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
   end Execute_Flow;

   procedure Execute_JR_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_JR);

   procedure Execute_JR_NZ_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_JR,
      Condition => Instructions.JCOND_NZ);

   procedure Execute_JR_Z_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_JR,
      Condition => Instructions.JCOND_Z);

   procedure Execute_JR_NC_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_JR,
      Condition => Instructions.JCOND_NC);

   procedure Execute_JR_C_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_JR,
      Condition => Instructions.JCOND_C);

   procedure Execute_JP_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_JP);

   procedure Execute_JP_NZ_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_JP,
      Condition => Instructions.JCOND_NZ);

   procedure Execute_JP_Z_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_JP,
      Condition => Instructions.JCOND_Z);

   procedure Execute_JP_NC_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_JP,
      Condition => Instructions.JCOND_NC);

   procedure Execute_JP_C_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_JP,
      Condition => Instructions.JCOND_C);

   procedure Execute_JP_HL_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_JP,
      Target    => Instructions.JTARGET_HL);

   procedure Execute_CALL_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_CALL);

   procedure Execute_CALL_NZ_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_CALL,
      Condition => Instructions.JCOND_NZ);

   procedure Execute_CALL_Z_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_CALL,
      Condition => Instructions.JCOND_Z);

   procedure Execute_CALL_NC_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_CALL,
      Condition => Instructions.JCOND_NC);

   procedure Execute_CALL_C_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_CALL,
      Condition => Instructions.JCOND_C);

   procedure Execute_RET_NZ_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RET,
      Condition => Instructions.JCOND_NZ);

   procedure Execute_RET_Z_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RET,
      Condition => Instructions.JCOND_Z);

   procedure Execute_RET_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RET);

   procedure Execute_RET_NC_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RET,
      Condition => Instructions.JCOND_NC);

   procedure Execute_RET_C_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RET,
      Condition => Instructions.JCOND_C);

   procedure Execute_RETI_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RETI);

   procedure Execute_RST_00_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0000#);

   procedure Execute_RST_08_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0008#);

   procedure Execute_RST_10_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0010#);

   procedure Execute_RST_18_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0018#);

   procedure Execute_RST_20_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0020#);

   procedure Execute_RST_28_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0028#);

   procedure Execute_RST_30_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0030#);

   procedure Execute_RST_38_Impl is new Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0038#);

   procedure Execute_JR
     (GB : in out Gade.GB.GB_Type) renames Execute_JR_Impl;
   procedure Execute_JR_NZ
     (GB : in out Gade.GB.GB_Type) renames Execute_JR_NZ_Impl;
   procedure Execute_JR_Z
     (GB : in out Gade.GB.GB_Type) renames Execute_JR_Z_Impl;
   procedure Execute_JR_NC
     (GB : in out Gade.GB.GB_Type) renames Execute_JR_NC_Impl;
   procedure Execute_JR_C
     (GB : in out Gade.GB.GB_Type) renames Execute_JR_C_Impl;
   procedure Execute_JP
     (GB : in out Gade.GB.GB_Type) renames Execute_JP_Impl;
   procedure Execute_JP_NZ
     (GB : in out Gade.GB.GB_Type) renames Execute_JP_NZ_Impl;
   procedure Execute_JP_Z
     (GB : in out Gade.GB.GB_Type) renames Execute_JP_Z_Impl;
   procedure Execute_JP_NC
     (GB : in out Gade.GB.GB_Type) renames Execute_JP_NC_Impl;
   procedure Execute_JP_C
     (GB : in out Gade.GB.GB_Type) renames Execute_JP_C_Impl;
   procedure Execute_JP_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_JP_HL_Impl;
   procedure Execute_CALL
     (GB : in out Gade.GB.GB_Type) renames Execute_CALL_Impl;
   procedure Execute_CALL_NZ
     (GB : in out Gade.GB.GB_Type) renames Execute_CALL_NZ_Impl;
   procedure Execute_CALL_Z
     (GB : in out Gade.GB.GB_Type) renames Execute_CALL_Z_Impl;
   procedure Execute_CALL_NC
     (GB : in out Gade.GB.GB_Type) renames Execute_CALL_NC_Impl;
   procedure Execute_CALL_C
     (GB : in out Gade.GB.GB_Type) renames Execute_CALL_C_Impl;
   procedure Execute_RET_NZ
     (GB : in out Gade.GB.GB_Type) renames Execute_RET_NZ_Impl;
   procedure Execute_RET_Z
     (GB : in out Gade.GB.GB_Type) renames Execute_RET_Z_Impl;
   procedure Execute_RET
     (GB : in out Gade.GB.GB_Type) renames Execute_RET_Impl;
   procedure Execute_RET_NC
     (GB : in out Gade.GB.GB_Type) renames Execute_RET_NC_Impl;
   procedure Execute_RET_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RET_C_Impl;
   procedure Execute_RETI
     (GB : in out Gade.GB.GB_Type) renames Execute_RETI_Impl;
   procedure Execute_RST_00
     (GB : in out Gade.GB.GB_Type) renames Execute_RST_00_Impl;
   procedure Execute_RST_08
     (GB : in out Gade.GB.GB_Type) renames Execute_RST_08_Impl;
   procedure Execute_RST_10
     (GB : in out Gade.GB.GB_Type) renames Execute_RST_10_Impl;
   procedure Execute_RST_18
     (GB : in out Gade.GB.GB_Type) renames Execute_RST_18_Impl;
   procedure Execute_RST_20
     (GB : in out Gade.GB.GB_Type) renames Execute_RST_20_Impl;
   procedure Execute_RST_28
     (GB : in out Gade.GB.GB_Type) renames Execute_RST_28_Impl;
   procedure Execute_RST_30
     (GB : in out Gade.GB.GB_Type) renames Execute_RST_30_Impl;
   procedure Execute_RST_38
     (GB : in out Gade.GB.GB_Type) renames Execute_RST_38_Impl;

end Gade.Dev.CPU.Instructions.Flow;
