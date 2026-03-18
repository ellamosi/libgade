with Gade.Dev.CPU.Arithmetic; use Gade.Dev.CPU.Arithmetic;
with Gade.Dev.CPU.Bitwise;    use Gade.Dev.CPU.Bitwise;
with Gade.Dev.CPU.Cycle_Steps;
with Gade.Dev.CPU.Logic;      use Gade.Dev.CPU.Logic;
with Gade.GB.Memory_Map;      use Gade.GB.Memory_Map;

package body Gade.Dev.CPU.Generic_Handlers is

   function Bus_Read_Byte
     (GB      : in out Gade.GB.GB_Type;
      Address :        Word) return Byte;

   procedure Bus_Write_Byte
     (GB      : in out Gade.GB.GB_Type;
      Address :        Word;
      Value   :        Byte);

   procedure Internal_Cycle
     (GB : in out Gade.GB.GB_Type);

   function Fetch_Source
     (GB     : in out Gade.GB.GB_Type;
      Source :        Byte_Source_Kind) return Byte;

   procedure Store_Target
     (GB     : in out Gade.GB.GB_Type;
      Target :        Byte_Target_Kind;
      Value  :        Byte);

   function Load_Target
     (GB     : in out Gade.GB.GB_Type;
      Target :        Byte_Target_Kind) return Byte;

   function Fetch_Imm8
     (GB : in out Gade.GB.GB_Type) return Byte;

   function Fetch_Imm16
     (GB : in out Gade.GB.GB_Type) return Word;

   function Read_Word_Register
     (GB     : Gade.GB.GB_Type;
      Target : Word_Register_Kind) return Word;

   function Read_Word_Source
     (GB     : in out Gade.GB.GB_Type;
      Source :        Word_Source_Kind) return Word;

   procedure Write_Word_Register
     (GB     : in out Gade.GB.GB_Type;
      Target :        Word_Register_Kind;
      Value  :        Word);

   procedure Push_Word
     (GB    : in out Gade.GB.GB_Type;
      Value :        Word);

   procedure Pop_Word
     (GB    : in out Gade.GB.GB_Type;
      Value :    out Word);

   procedure Adjust_HL_Auto
     (GB     : in out Gade.GB.GB_Type;
      Source :        Byte_Source_Kind);

   procedure Adjust_HL_Auto
     (GB     : in out Gade.GB.GB_Type;
      Target :        Byte_Target_Kind);

   function Check_Condition
     (CPU       : CPU_Context;
      Condition : Jump_Condition_Kind) return Boolean;

   function Bus_Read_Byte
     (GB      : in out Gade.GB.GB_Type;
      Address :        Word) return Byte is
   begin
      Gade.Dev.CPU.Cycle_Steps.Step_M_Cycle (GB.CPU);
      return Read_Byte (GB, Address);
   end Bus_Read_Byte;

   procedure Bus_Write_Byte
     (GB      : in out Gade.GB.GB_Type;
      Address :        Word;
      Value   :        Byte) is
   begin
      Gade.Dev.CPU.Cycle_Steps.Step_M_Cycle (GB.CPU);
      Write_Byte (GB, Address, Value);
   end Bus_Write_Byte;

   procedure Internal_Cycle
     (GB : in out Gade.GB.GB_Type) is
   begin
      Gade.Dev.CPU.Cycle_Steps.Step_M_Cycle (GB.CPU);
   end Internal_Cycle;

   function Fetch_Source
     (GB     : in out Gade.GB.GB_Type;
      Source :        Byte_Source_Kind) return Byte is
   begin
      case Source is
         when SRC_A =>
            return GB.CPU.Regs.A;
         when SRC_B =>
            return GB.CPU.Regs.B;
         when SRC_C =>
            return GB.CPU.Regs.C;
         when SRC_D =>
            return GB.CPU.Regs.D;
         when SRC_E =>
            return GB.CPU.Regs.E;
         when SRC_H =>
            return GB.CPU.Regs.H;
         when SRC_L =>
            return GB.CPU.Regs.L;
         when SRC_Addr_BC =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.BC);
         when SRC_Addr_DE =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.DE);
         when SRC_Addr_HL =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.HL);
         when SRC_Addr_HL_Inc | SRC_Addr_HL_Dec =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.HL);
         when SRC_Addr_Imm16 =>
            return Bus_Read_Byte (GB, Fetch_Imm16 (GB));
         when SRC_High_Addr_C =>
            return Bus_Read_Byte (GB, 16#FF00# + Word (GB.CPU.Regs.C));
         when SRC_High_Addr_Imm8 =>
            return Bus_Read_Byte (GB, 16#FF00# + Word (Fetch_Imm8 (GB)));
         when SRC_Imm8 =>
            return Fetch_Imm8 (GB);
      end case;
   end Fetch_Source;

   procedure Store_Target
     (GB     : in out Gade.GB.GB_Type;
      Target :        Byte_Target_Kind;
      Value  :        Byte) is
   begin
      case Target is
         when DST_A =>
            GB.CPU.Regs.A := Value;
         when DST_B =>
            GB.CPU.Regs.B := Value;
         when DST_C =>
            GB.CPU.Regs.C := Value;
         when DST_D =>
            GB.CPU.Regs.D := Value;
         when DST_E =>
            GB.CPU.Regs.E := Value;
         when DST_H =>
            GB.CPU.Regs.H := Value;
         when DST_L =>
            GB.CPU.Regs.L := Value;
         when DST_Addr_BC =>
            Bus_Write_Byte (GB, GB.CPU.Regs.BC, Value);
         when DST_Addr_DE =>
            Bus_Write_Byte (GB, GB.CPU.Regs.DE, Value);
         when DST_Addr_HL =>
            Bus_Write_Byte (GB, GB.CPU.Regs.HL, Value);
         when DST_Addr_HL_Inc | DST_Addr_HL_Dec =>
            Bus_Write_Byte (GB, GB.CPU.Regs.HL, Value);
         when DST_Addr_Imm16 =>
            Bus_Write_Byte (GB, Fetch_Imm16 (GB), Value);
         when DST_High_Addr_C =>
            Bus_Write_Byte (GB, 16#FF00# + Word (GB.CPU.Regs.C), Value);
         when DST_High_Addr_Imm8 =>
            Bus_Write_Byte (GB, 16#FF00# + Word (Fetch_Imm8 (GB)), Value);
      end case;
   end Store_Target;

   function Load_Target
     (GB     : in out Gade.GB.GB_Type;
      Target :        Byte_Target_Kind) return Byte is
   begin
      case Target is
         when DST_A =>
            return GB.CPU.Regs.A;
         when DST_B =>
            return GB.CPU.Regs.B;
         when DST_C =>
            return GB.CPU.Regs.C;
         when DST_D =>
            return GB.CPU.Regs.D;
         when DST_E =>
            return GB.CPU.Regs.E;
         when DST_H =>
            return GB.CPU.Regs.H;
         when DST_L =>
            return GB.CPU.Regs.L;
         when DST_Addr_BC =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.BC);
         when DST_Addr_DE =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.DE);
         when DST_Addr_HL | DST_Addr_HL_Inc | DST_Addr_HL_Dec =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.HL);
         when DST_Addr_Imm16 =>
            return Bus_Read_Byte (GB, Fetch_Imm16 (GB));
         when DST_High_Addr_C =>
            return Bus_Read_Byte (GB, 16#FF00# + Word (GB.CPU.Regs.C));
         when DST_High_Addr_Imm8 =>
            return Bus_Read_Byte (GB, 16#FF00# + Word (Fetch_Imm8 (GB)));
      end case;
   end Load_Target;

   function Fetch_Imm8
     (GB : in out Gade.GB.GB_Type) return Byte is
      Value : constant Byte := Bus_Read_Byte (GB, GB.CPU.PC);
   begin
      GB.CPU.PC := GB.CPU.PC + 1;
      return Value;
   end Fetch_Imm8;

   function Fetch_Imm16
     (GB : in out Gade.GB.GB_Type) return Word is
      Lo : constant Byte := Fetch_Imm8 (GB);
      Hi : constant Byte := Fetch_Imm8 (GB);
   begin
      return Word (Lo) + Word (Hi) * 2**8;
   end Fetch_Imm16;

   function Read_Word_Register
     (GB     : Gade.GB.GB_Type;
      Target : Word_Register_Kind) return Word is
   begin
      case Target is
         when REG_AF =>
            return GB.CPU.Regs.AF;
         when REG_BC =>
            return GB.CPU.Regs.BC;
         when REG_DE =>
            return GB.CPU.Regs.DE;
         when REG_HL =>
            return GB.CPU.Regs.HL;
         when REG_SP =>
            return GB.CPU.Regs.SP;
      end case;
   end Read_Word_Register;

   function Read_Word_Source
     (GB     : in out Gade.GB.GB_Type;
      Source :        Word_Source_Kind) return Word is
   begin
      case Source is
         when WSRC_BC =>
            return GB.CPU.Regs.BC;
         when WSRC_DE =>
            return GB.CPU.Regs.DE;
         when WSRC_HL =>
            return GB.CPU.Regs.HL;
         when WSRC_SP =>
            return GB.CPU.Regs.SP;
         when WSRC_Imm16 =>
            return Fetch_Imm16 (GB);
      end case;
   end Read_Word_Source;

   procedure Write_Word_Register
     (GB     : in out Gade.GB.GB_Type;
      Target :        Word_Register_Kind;
      Value  :        Word) is
   begin
      case Target is
         when REG_AF =>
            GB.CPU.Regs.AF := Value and 16#FFF0#;
         when REG_BC =>
            GB.CPU.Regs.BC := Value;
         when REG_DE =>
            GB.CPU.Regs.DE := Value;
         when REG_HL =>
            GB.CPU.Regs.HL := Value;
         when REG_SP =>
            GB.CPU.Regs.SP := Value;
      end case;
   end Write_Word_Register;

   procedure Push_Word
     (GB    : in out Gade.GB.GB_Type;
      Value :        Word) is
   begin
      Push (GB, Value);
   end Push_Word;

   procedure Pop_Word
     (GB    : in out Gade.GB.GB_Type;
      Value :    out Word) is
   begin
      Pop (GB, Value);
   end Pop_Word;

   procedure Adjust_HL_Auto
     (GB     : in out Gade.GB.GB_Type;
      Source :        Byte_Source_Kind) is
   begin
      case Source is
         when SRC_Addr_HL_Inc =>
            GB.CPU.Regs.HL := GB.CPU.Regs.HL + 1;
         when SRC_Addr_HL_Dec =>
            GB.CPU.Regs.HL := GB.CPU.Regs.HL - 1;
         when others =>
            null;
      end case;
   end Adjust_HL_Auto;

   procedure Adjust_HL_Auto
     (GB     : in out Gade.GB.GB_Type;
      Target :        Byte_Target_Kind) is
   begin
      case Target is
         when DST_Addr_HL_Inc =>
            GB.CPU.Regs.HL := GB.CPU.Regs.HL + 1;
         when DST_Addr_HL_Dec =>
            GB.CPU.Regs.HL := GB.CPU.Regs.HL - 1;
         when others =>
            null;
      end case;
   end Adjust_HL_Auto;

   function Check_Condition
     (CPU       : CPU_Context;
      Condition : Jump_Condition_Kind) return Boolean is
   begin
      case Condition is
         when JCOND_None =>
            return True;
         when JCOND_NZ =>
            return Gade.Dev.CPU.Check_Condition (CPU, C_NZ);
         when JCOND_Z =>
            return Gade.Dev.CPU.Check_Condition (CPU, C_Z);
         when JCOND_NC =>
            return Gade.Dev.CPU.Check_Condition (CPU, C_NC);
         when JCOND_C =>
            return Gade.Dev.CPU.Check_Condition (CPU, C_C);
      end case;
   end Check_Condition;

   procedure Execute_ALU_A_Source
     (GB : in out Gade.GB.GB_Type) is
      Value : constant Byte := Fetch_Source (GB, Source);
      Dummy : Byte;
   begin
      case Operation is
         when ALU_ADD =>
            Do_Add (GB.CPU, Value, GB.CPU.Regs.A, ADD_Carry);
         when ALU_ADC =>
            Do_Add (GB.CPU, Value, GB.CPU.Regs.A, ADC_Carry);
         when ALU_SUB =>
            Do_Sub (GB.CPU, Value, GB.CPU.Regs.A, SUB_Carry);
         when ALU_SBC =>
            Do_Sub (GB.CPU, Value, GB.CPU.Regs.A, SBC_Carry);
         when ALU_AND =>
            Do_AND (GB.CPU, Value);
         when ALU_XOR =>
            Do_XOR (GB.CPU, Value);
         when ALU_OR =>
            Do_OR (GB.CPU, Value);
         when ALU_CP =>
            Do_Sub (GB.CPU, Value, Dummy, SUB_Carry);
      end case;
   end Execute_ALU_A_Source;

   procedure Execute_Bit_Source
     (GB : in out Gade.GB.GB_Type) is
      Value  : constant Byte := Fetch_Source (GB, Target);
      Result : Byte;
   begin
      case Operation is
         when BIT_Test =>
            Do_Bit (GB.CPU, Index, Value);
         when BIT_Set =>
            Do_Set_Bit (GB.CPU, SR_SET, Index, Value, Result);
            Store_Target (GB, Byte_Target_Kind'Val (Byte_Source_Kind'Pos (Target)), Result);
         when BIT_Reset =>
            Do_Set_Bit (GB.CPU, SR_RES, Index, Value, Result);
            Store_Target (GB, Byte_Target_Kind'Val (Byte_Source_Kind'Pos (Target)), Result);
      end case;
   end Execute_Bit_Source;

   procedure Execute_LD_Byte
     (GB : in out Gade.GB.GB_Type) is
      Value : constant Byte := Fetch_Source (GB, Source);
   begin
      Store_Target (GB, Dest, Value);
      Adjust_HL_Auto (GB, Source);
      Adjust_HL_Auto (GB, Dest);
   end Execute_LD_Byte;

   procedure Execute_LD_Word
     (GB : in out Gade.GB.GB_Type) is
   begin
      Write_Word_Register (GB, Dest, Read_Word_Source (GB, Source));
   end Execute_LD_Word;

   procedure Execute_LD_Addr_Imm16_SP
     (GB : in out Gade.GB.GB_Type) is
   begin
      declare
         Address : constant Word := Fetch_Imm16 (GB);
      begin
         Bus_Write_Byte (GB, Address, Byte (GB.CPU.Regs.SP and 16#00FF#));
         Bus_Write_Byte (GB, Address + 1, Byte (GB.CPU.Regs.SP / 2**8));
      end;
   end Execute_LD_Addr_Imm16_SP;

   procedure Execute_LD_HL_SP_Plus_Imm8
     (GB : in out Gade.GB.GB_Type) is
      Value : Word := GB.CPU.Regs.SP;
   begin
      Do_Add (GB.CPU, Value, Fetch_Imm8 (GB));
      GB.CPU.Regs.HL := Value;
   end Execute_LD_HL_SP_Plus_Imm8;

   procedure Execute_Inc_Dec_Byte
     (GB : in out Gade.GB.GB_Type) is
      Value : Byte := Load_Target (GB, Target);
   begin
      Do_Inc_Dec
        (GB.CPU,
         (if Operation = OP_INC then INC else DEC),
         Value,
         Value);
      Store_Target (GB, Target, Value);
   end Execute_Inc_Dec_Byte;

   procedure Execute_Inc_Dec_Word
     (GB : in out Gade.GB.GB_Type) is
      Value : Word := Read_Word_Register (GB, Target);
   begin
      if Operation = OP_INC then
         Value := Value + 1;
      else
         Value := Value - 1;
      end if;
      Write_Word_Register (GB, Target, Value);
   end Execute_Inc_Dec_Word;

   procedure Execute_Rotate_Shift
     (GB : in out Gade.GB.GB_Type) is
      Value : Byte := Load_Target (GB, Target);
   begin
      case Operation is
         when ROT_RLC =>
            Do_RLC (GB.CPU, Adjust_Flags, Value);
         when ROT_RRC =>
            Do_RRC (GB.CPU, Adjust_Flags, Value);
         when ROT_RL =>
            Do_RL (GB.CPU, Adjust_Flags, Value);
         when ROT_RR =>
            Do_RR (GB.CPU, Adjust_Flags, Value);
         when ROT_SLA =>
            Do_SL (GB.CPU, S_A, Value);
         when ROT_SRA =>
            Do_SR (GB.CPU, S_A, Value);
         when ROT_SWAP =>
            Do_Swap (GB.CPU, Value);
         when ROT_SRL =>
            Do_SR (GB.CPU, S_L, Value);
      end case;
      Store_Target (GB, Target, Value);
   end Execute_Rotate_Shift;

   procedure Execute_Push
     (GB : in out Gade.GB.GB_Type) is
   begin
      Push_Word (GB, Read_Word_Register (GB, Source));
   end Execute_Push;

   procedure Execute_Pop
     (GB : in out Gade.GB.GB_Type) is
      Value : Word;
   begin
      Pop_Word (GB, Value);
      Write_Word_Register (GB, Dest, Value);
   end Execute_Pop;

   procedure Execute_Flow
     (GB : in out Gade.GB.GB_Type) is
      Destination : Word;
      Offset      : Byte;
   begin
      case Operation is
         when FLOW_RET =>
            if Check_Condition (GB.CPU, Condition) then
               GB.CPU.Branch_Taken := Condition /= JCOND_None;
               Pop_Word (GB, GB.CPU.PC);
            end if;

         when FLOW_RETI =>
            Pop_Word (GB, GB.CPU.PC);
            GB.CPU.IFF := IE_EI;

         when FLOW_JR =>
            Offset := Fetch_Imm8 (GB);
            if Check_Condition (GB.CPU, Condition) then
               GB.CPU.Branch_Taken := Condition /= JCOND_None;
               Add_Offset (GB.CPU, GB.CPU.PC, Offset, False);
            end if;

         when FLOW_JP =>
            if Target = JTARGET_HL then
               if Check_Condition (GB.CPU, Condition) then
                  GB.CPU.PC := GB.CPU.Regs.HL;
               end if;
            else
               Destination := Fetch_Imm16 (GB);
               if Check_Condition (GB.CPU, Condition) then
                  GB.CPU.Branch_Taken := Condition /= JCOND_None;
                  GB.CPU.PC := Destination;
               end if;
            end if;

         when FLOW_CALL =>
            Destination := Fetch_Imm16 (GB);
            if Check_Condition (GB.CPU, Condition) then
               GB.CPU.Branch_Taken := Condition /= JCOND_None;
               Internal_Cycle (GB);
               Push_Word (GB, GB.CPU.PC);
               GB.CPU.PC := Destination;
            end if;

         when FLOW_RST =>
            Internal_Cycle (GB);
            Push_Word (GB, GB.CPU.PC);
            GB.CPU.PC := Vector;
      end case;
   end Execute_Flow;

   procedure Execute_NOP
     (GB : in out Gade.GB.GB_Type) is
      pragma Unreferenced (GB);
   begin
      null;
   end Execute_NOP;

   procedure Execute_Add_HL
     (GB : in out Gade.GB.GB_Type) is
      Value : Word := GB.CPU.Regs.HL;
   begin
      Do_Add (GB.CPU, Read_Word_Register (GB, Source), Value);
      Internal_Cycle (GB);
      GB.CPU.Regs.HL := Value;
   end Execute_Add_HL;

   procedure Execute_Add_SP_Imm8
     (GB : in out Gade.GB.GB_Type) is
   begin
      Do_Add (GB.CPU, GB.CPU.Regs.SP, Fetch_Imm8 (GB));
      Internal_Cycle (GB);
   end Execute_Add_SP_Imm8;

   procedure Execute_DAA
     (GB : in out Gade.GB.GB_Type) is
   begin
      Do_Daa (GB.CPU);
   end Execute_DAA;

   procedure Execute_CPL
     (GB : in out Gade.GB.GB_Type) is
   begin
      GB.CPU.Regs.A := not GB.CPU.Regs.A;
      Set (GB.CPU.Regs.F.H);
      Set (GB.CPU.Regs.F.N);
   end Execute_CPL;

   procedure Execute_SCF
     (GB : in out Gade.GB.GB_Type) is
   begin
      Reset (GB.CPU.Regs.F.N);
      Reset (GB.CPU.Regs.F.H);
      Set (GB.CPU.Regs.F.C);
   end Execute_SCF;

   procedure Execute_CCF
     (GB : in out Gade.GB.GB_Type) is
   begin
      Reset (GB.CPU.Regs.F.N);
      Reset (GB.CPU.Regs.F.H);
      Set_Value (GB.CPU.Regs.F.C, not Is_Set (GB.CPU.Regs.F.C));
   end Execute_CCF;

   procedure Execute_HALT
     (GB : in out Gade.GB.GB_Type) is
   begin
      GB.CPU.Halted := True;
   end Execute_HALT;

   procedure Execute_STOP
     (GB : in out Gade.GB.GB_Type) is
   begin
      GB.CPU.PC := GB.CPU.PC + 1;
      if GB.CPU.IFF = IE_EI then
         GB.CPU.Halted := True;
      end if;
   end Execute_STOP;

   procedure Execute_DI
     (GB : in out Gade.GB.GB_Type) is
   begin
      GB.CPU.IFF := IE_DI;
   end Execute_DI;

   procedure Execute_EI
     (GB : in out Gade.GB.GB_Type) is
   begin
      GB.CPU.IFF := IE_EI;
   end Execute_EI;

end Gade.Dev.CPU.Generic_Handlers;
