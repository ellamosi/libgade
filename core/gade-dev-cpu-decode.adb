with Gade.Dev.CPU.Instructions.Table; use Gade.Dev.CPU.Instructions.Table;
with Gade.GB;                         use Gade.GB;
with Gade.GB.Memory_Map;              use Gade.GB.Memory_Map;

package body Gade.Dev.CPU.Decode is

   type Half_Opcode_Field is range 0 .. 3;
   type Opcode_Field is range 0 .. 7;
   type Quarter_Opcode_Field is range 0 .. 1;

   Condition_Map : constant array (Half_Opcode_Field) of Condition_Kind :=
     [0 => COND_NZ, 1 => COND_Z, 2 => COND_NC, 3 => COND_C];

   Register16_Map : constant array (Half_Opcode_Field) of Operand_Kind :=
     [0 => OD_BC, 1 => OD_DE, 2 => OD_HL, 3 => OD_SP];

   Register16_Stack_Map :
     constant array (Half_Opcode_Field) of Operand_Kind :=
       [0 => OD_BC, 1 => OD_DE, 2 => OD_HL, 3 => OD_AF];

   Register8_Map : constant array (Opcode_Field) of Operand_Kind :=
     [0 => OD_B, 1 => OD_C, 2 => OD_D, 3 => OD_E,
      4 => OD_H, 5 => OD_L, 6 => OD_Addr_HL, 7 => OD_A];

   procedure Apply_Timing
     (Inst : in out Decoded_Instruction;
      Table_Entry : Instruction_Entry);

   procedure Decode_CB
     (Opcode : Byte;
      Inst   : in out Decoded_Instruction);

   procedure Decode_Main
     (Opcode : Byte;
      Inst   : in out Decoded_Instruction);

   function Field_X (Opcode : Byte) return Half_Opcode_Field;
   function Field_Y (Opcode : Byte) return Opcode_Field;
   function Field_Z (Opcode : Byte) return Opcode_Field;
   function Field_P (Opcode : Byte) return Half_Opcode_Field;
   function Field_Q (Opcode : Byte) return Quarter_Opcode_Field;

   function Lookup_Entry
     (Prefix : Prefix_Kind;
      Opcode : Byte) return Instruction_Entry;

   function To_Rel8 (Value : Byte) return Signed_Byte;

   procedure Apply_Timing
     (Inst : in out Decoded_Instruction;
      Table_Entry : Instruction_Entry) is
   begin
      Inst.Cycles := Table_Entry.Cycles;
      Inst.Jump_Cycles := Table_Entry.Jump_Cycles;
   end Apply_Timing;

   function Decode
     (GB : in out Gade.GB.GB_Type) return Decoded_Instruction
   is
      Inst   : Decoded_Instruction;
      Opcode : constant Byte := Read_Byte (GB, GB.CPU.PC);
   begin
      if Opcode = 16#CB# then
         Inst.Prefix := CB;
         Inst.Length := 2;
         Inst.Opcode := Read_Byte (GB, GB.CPU.PC + 1);
         Decode_CB (Inst.Opcode, Inst);
         Apply_Timing (Inst, Lookup_Entry (CB, Inst.Opcode));
      else
         Inst.Opcode := Opcode;
         Decode_Main (Opcode, Inst);

         case Inst.Length is
            when 2 =>
               Inst.Imm8 := Read_Byte (GB, GB.CPU.PC + 1);
               Inst.Rel8 := To_Rel8 (Inst.Imm8);
            when 3 =>
               Inst.Imm8 := Read_Byte (GB, GB.CPU.PC + 1);
               Inst.Imm16 := Read_Word (GB, GB.CPU.PC + 1);
               if Inst.Src = OD_Rel8 or else Inst.Dest = OD_Rel8 then
                  Inst.Rel8 := To_Rel8 (Inst.Imm8);
               end if;
            when others =>
               null;
         end case;

         Apply_Timing (Inst, Lookup_Entry (Main, Opcode));
      end if;

      return Inst;
   end Decode;

   procedure Decode_CB
     (Opcode : Byte;
      Inst   : in out Decoded_Instruction)
   is
      X : constant Half_Opcode_Field := Field_X (Opcode);
      Y : constant Opcode_Field := Field_Y (Opcode);
      Z : constant Opcode_Field := Field_Z (Opcode);
   begin
      Inst.Dest := Register8_Map (Z);

      case X is
         when 0 =>
            Inst.Operation :=
              (case Y is
                  when 0 => OP_RLC,
                  when 1 => OP_RRC,
                  when 2 => OP_RL,
                  when 3 => OP_RR,
                  when 4 => OP_SLA,
                  when 5 => OP_SRA,
                  when 6 => OP_SWAP,
                  when 7 => OP_SRL);
         when 1 =>
            Inst.Operation := OP_BIT;
            Inst.Src := Inst.Dest;
            Inst.Dest := OD_Bit_Index;
            Inst.Bit_Index := Natural (Y);
         when 2 =>
            Inst.Operation := OP_RES;
            Inst.Src := Inst.Dest;
            Inst.Bit_Index := Natural (Y);
         when 3 =>
            Inst.Operation := OP_SET;
            Inst.Src := Inst.Dest;
            Inst.Bit_Index := Natural (Y);
      end case;
   end Decode_CB;

   procedure Decode_Main
     (Opcode : Byte;
      Inst   : in out Decoded_Instruction)
   is
      X : constant Half_Opcode_Field := Field_X (Opcode);
      Y : constant Opcode_Field := Field_Y (Opcode);
      Z : constant Opcode_Field := Field_Z (Opcode);
      P : constant Half_Opcode_Field := Field_P (Opcode);
      Q : constant Quarter_Opcode_Field := Field_Q (Opcode);
   begin
      case X is
         when 0 =>
            case Z is
               when 0 =>
                  case Y is
                     when 0 =>
                        Inst.Operation := OP_NOP;
                     when 1 =>
                        Inst.Operation := OP_LD;
                        Inst.Dest := OD_Addr_Imm16;
                        Inst.Src := OD_SP;
                        Inst.Length := 3;
                     when 2 =>
                        Inst.Operation := OP_STOP;
                        Inst.Length := 2;
                     when 3 =>
                        Inst.Operation := OP_JR;
                        Inst.Src := OD_Rel8;
                        Inst.Length := 2;
                     when others =>
                        Inst.Operation := OP_JR;
                        Inst.Condition := Condition_Map (Half_Opcode_Field (Y - 4));
                        Inst.Src := OD_Rel8;
                        Inst.Length := 2;
                  end case;
               when 1 =>
                  if Q = 0 then
                     Inst.Operation := OP_LD;
                     Inst.Dest := Register16_Map (P);
                     Inst.Src := OD_Imm16;
                     Inst.Length := 3;
                  else
                     Inst.Operation := OP_ADD;
                     Inst.Dest := OD_HL;
                     Inst.Src := Register16_Map (P);
                  end if;
               when 2 =>
                  if Q = 0 then
                     Inst.Operation := OP_LD;
                     Inst.Src := OD_A;
                     case P is
                        when 0 => Inst.Dest := OD_Addr_BC;
                        when 1 => Inst.Dest := OD_Addr_DE;
                        when 2 => Inst.Dest := OD_Addr_HL_Inc;
                        when 3 => Inst.Dest := OD_Addr_HL_Dec;
                     end case;
                  else
                     Inst.Operation := OP_LD;
                     Inst.Dest := OD_A;
                     case P is
                        when 0 => Inst.Src := OD_Addr_BC;
                        when 1 => Inst.Src := OD_Addr_DE;
                        when 2 => Inst.Src := OD_Addr_HL_Inc;
                        when 3 => Inst.Src := OD_Addr_HL_Dec;
                     end case;
                  end if;
               when 3 =>
                  Inst.Operation := (if Q = 0 then OP_INC else OP_DEC);
                  Inst.Dest := Register16_Map (P);
               when 4 =>
                  Inst.Operation := OP_INC;
                  Inst.Dest := Register8_Map (Y);
               when 5 =>
                  Inst.Operation := OP_DEC;
                  Inst.Dest := Register8_Map (Y);
               when 6 =>
                  Inst.Operation := OP_LD;
                  Inst.Dest := Register8_Map (Y);
                  Inst.Src := OD_Imm8;
                  Inst.Length := 2;
               when 7 =>
                  case Y is
                     when 0 => Inst.Operation := OP_RLCA;
                     when 1 => Inst.Operation := OP_RRCA;
                     when 2 => Inst.Operation := OP_RLA;
                     when 3 => Inst.Operation := OP_RRA;
                     when 4 => Inst.Operation := OP_DAA;
                     when 5 => Inst.Operation := OP_CPL;
                     when 6 => Inst.Operation := OP_SCF;
                     when 7 => Inst.Operation := OP_CCF;
                  end case;
            end case;
         when 1 =>
            if Y = 6 and then Z = 6 then
               Inst.Operation := OP_HALT;
            else
               Inst.Operation := OP_LD;
               Inst.Dest := Register8_Map (Y);
               Inst.Src := Register8_Map (Z);
            end if;
         when 2 =>
            Inst.Dest := OD_A;
            Inst.Src := Register8_Map (Z);
            Inst.Operation :=
              (case Y is
                  when 0 => OP_ADD,
                  when 1 => OP_ADC,
                  when 2 => OP_SUB,
                  when 3 => OP_SBC,
                  when 4 => OP_AND,
                  when 5 => OP_XOR,
                  when 6 => OP_OR,
                  when 7 => OP_CP);
         when 3 =>
            case Z is
               when 0 =>
                  case Y is
                     when 0 .. 3 =>
                        Inst.Operation := OP_RET;
                        Inst.Condition := Condition_Map (Half_Opcode_Field (Y));
                     when 4 =>
                        Inst.Operation := OP_LD;
                        Inst.Dest := OD_High_Addr_Imm8;
                        Inst.Src := OD_A;
                        Inst.Length := 2;
                     when 5 =>
                        Inst.Operation := OP_ADD;
                        Inst.Dest := OD_SP;
                        Inst.Src := OD_Rel8;
                        Inst.Length := 2;
                     when 6 =>
                        Inst.Operation := OP_LD;
                        Inst.Dest := OD_A;
                        Inst.Src := OD_High_Addr_Imm8;
                        Inst.Length := 2;
                     when 7 =>
                        Inst.Operation := OP_LD;
                        Inst.Dest := OD_HL;
                        Inst.Src := OD_SP_Plus_Rel8;
                        Inst.Length := 2;
                  end case;
               when 1 =>
                  if Q = 0 then
                     Inst.Operation := OP_POP;
                     Inst.Dest := Register16_Stack_Map (P);
                  else
                     case P is
                        when 0 =>
                           Inst.Operation := OP_RET;
                        when 1 =>
                           Inst.Operation := OP_RETI;
                        when 2 =>
                           Inst.Operation := OP_JP;
                           Inst.Src := OD_HL;
                        when 3 =>
                           Inst.Operation := OP_LD;
                           Inst.Dest := OD_SP;
                           Inst.Src := OD_HL;
                     end case;
                  end if;
               when 2 =>
                  case Y is
                     when 0 .. 3 =>
                        Inst.Operation := OP_JP;
                        Inst.Condition := Condition_Map (Half_Opcode_Field (Y));
                        Inst.Src := OD_Imm16;
                        Inst.Length := 3;
                     when 4 =>
                        Inst.Operation := OP_LD;
                        Inst.Dest := OD_High_Addr_C;
                        Inst.Src := OD_A;
                     when 5 =>
                        Inst.Operation := OP_LD;
                        Inst.Dest := OD_Addr_Imm16;
                        Inst.Src := OD_A;
                        Inst.Length := 3;
                     when 6 =>
                        Inst.Operation := OP_LD;
                        Inst.Dest := OD_A;
                        Inst.Src := OD_High_Addr_C;
                     when 7 =>
                        Inst.Operation := OP_LD;
                        Inst.Dest := OD_A;
                        Inst.Src := OD_Addr_Imm16;
                        Inst.Length := 3;
                  end case;
               when 3 =>
                  case Y is
                     when 0 =>
                        Inst.Operation := OP_JP;
                        Inst.Src := OD_Imm16;
                        Inst.Length := 3;
                     when 1 =>
                        Inst.Prefix := CB;
                        Inst.Operation := OP_Invalid;
                     when 6 =>
                        Inst.Operation := OP_DI;
                     when 7 =>
                        Inst.Operation := OP_EI;
                     when others =>
                        null;
                  end case;
               when 4 =>
                  if Y <= 3 then
                     Inst.Operation := OP_CALL;
                     Inst.Condition := Condition_Map (Half_Opcode_Field (Y));
                     Inst.Src := OD_Imm16;
                     Inst.Length := 3;
                  end if;
               when 5 =>
                  if Q = 0 then
                     Inst.Operation := OP_PUSH;
                     Inst.Src := Register16_Stack_Map (P);
                  elsif P = 0 then
                     Inst.Operation := OP_CALL;
                     Inst.Src := OD_Imm16;
                     Inst.Length := 3;
                  end if;
               when 6 =>
                  Inst.Dest := OD_A;
                  Inst.Src := OD_Imm8;
                  Inst.Length := 2;
                  Inst.Operation :=
                    (case Y is
                        when 0 => OP_ADD,
                        when 1 => OP_ADC,
                        when 2 => OP_SUB,
                        when 3 => OP_SBC,
                        when 4 => OP_AND,
                        when 5 => OP_XOR,
                        when 6 => OP_OR,
                        when 7 => OP_CP);
               when 7 =>
                  Inst.Operation := OP_RST;
                  Inst.Dest := OD_RST_Vector;
                  Inst.RST_Vector := Word (Natural (Y) * 8);
            end case;
      end case;
   end Decode_Main;

   function Field_P (Opcode : Byte) return Half_Opcode_Field is
   begin
      return Half_Opcode_Field (Field_Y (Opcode) / 2);
   end Field_P;

   function Field_Q (Opcode : Byte) return Quarter_Opcode_Field is
   begin
      return Quarter_Opcode_Field (Field_Y (Opcode) mod 2);
   end Field_Q;

   function Field_X (Opcode : Byte) return Half_Opcode_Field is
   begin
      return Half_Opcode_Field (Opcode / 64);
   end Field_X;

   function Field_Y (Opcode : Byte) return Opcode_Field is
   begin
      return Opcode_Field ((Opcode / 8) mod 8);
   end Field_Y;

   function Field_Z (Opcode : Byte) return Opcode_Field is
   begin
      return Opcode_Field (Opcode mod 8);
   end Field_Z;

   function Lookup_Entry
     (Prefix : Prefix_Kind;
      Opcode : Byte) return Instruction_Entry is
   begin
      case Prefix is
         when Main =>
            return Opcodes_Main.Entries (Opcode);
         when CB =>
            return Opcodes_CB.Entries (Opcode);
      end case;
   end Lookup_Entry;

   function To_Rel8 (Value : Byte) return Signed_Byte is
      Integer_Value : constant Integer := Integer (Value);
   begin
      if Integer_Value <= 127 then
         return Signed_Byte (Integer_Value);
      else
         return Signed_Byte (Integer_Value - 256);
      end if;
   end To_Rel8;

end Gade.Dev.CPU.Decode;
