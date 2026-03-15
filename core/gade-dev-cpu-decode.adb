with Gade.Dev.CPU.Instructions.Table; use Gade.Dev.CPU.Instructions.Table;
with Gade.GB;                         use Gade.GB;
with Gade.GB.Memory_Map;              use Gade.GB.Memory_Map;

package body Gade.Dev.CPU.Decode is

   type Half_Opcode_Field is range 0 .. 3;
   type Opcode_Field is range 0 .. 7;
   type Quarter_Opcode_Field is range 0 .. 1;

   type Decoded_Instruction_Table is array (Byte) of Decoded_Instruction;

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

   ALU_Operation_Map : constant array (Opcode_Field) of Operation_Kind :=
     [0 => OP_ADD, 1 => OP_ADC, 2 => OP_SUB, 3 => OP_SBC,
      4 => OP_AND, 5 => OP_XOR, 6 => OP_OR, 7 => OP_CP];

   CB_Rotate_Operation_Map : constant array (Opcode_Field) of Operation_Kind :=
     [0 => OP_RLC, 1 => OP_RRC, 2 => OP_RL, 3 => OP_RR,
      4 => OP_SLA, 5 => OP_SRA, 6 => OP_SWAP, 7 => OP_SRL];

   Accumulator_Rotate_Operation_Map :
     constant array (Half_Opcode_Field range 0 .. 3) of Operation_Kind :=
       [0 => OP_RLCA, 1 => OP_RRCA, 2 => OP_RLA, 3 => OP_RRA];

   function Apply_Prefix
     (Inst   : Decoded_Instruction;
      Prefix : Prefix_Kind;
      Opcode : Byte) return Decoded_Instruction;

   procedure Apply_Timing
     (Inst        : in out Decoded_Instruction;
      Table_Entry : Instruction_Entry);

   function Build_CB_Table return Decoded_Instruction_Table;
   function Build_Main_Table return Decoded_Instruction_Table;

   function Field_P (Opcode : Byte) return Half_Opcode_Field;
   function Field_Q (Opcode : Byte) return Quarter_Opcode_Field;
   function Field_X (Opcode : Byte) return Half_Opcode_Field;
   function Field_Y (Opcode : Byte) return Opcode_Field;
   function Field_Z (Opcode : Byte) return Opcode_Field;

   function Lookup_Entry
     (Prefix : Prefix_Kind;
      Opcode : Byte) return Instruction_Entry;

   function Make
     (Operation   : Operation_Kind;
      Dest        : Operand_Kind := OD_None;
      Src         : Operand_Kind := OD_None;
      Condition   : Condition_Kind := COND_None;
      Length      : Positive := 1;
      Bit_Index   : Natural := 0;
      RST_Vector  : Word := 0) return Decoded_Instruction;

   function To_Rel8 (Value : Byte) return Signed_Byte;

   function Apply_Prefix
     (Inst   : Decoded_Instruction;
      Prefix : Prefix_Kind;
      Opcode : Byte) return Decoded_Instruction
   is
      Result : Decoded_Instruction := Inst;
   begin
      Result.Prefix := Prefix;
      Result.Opcode := Opcode;
      return Result;
   end Apply_Prefix;

   procedure Apply_Timing
     (Inst        : in out Decoded_Instruction;
      Table_Entry : Instruction_Entry) is
   begin
      Inst.Cycles := Table_Entry.Cycles;
      Inst.Jump_Cycles := Table_Entry.Jump_Cycles;
   end Apply_Timing;

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

   function Make
     (Operation   : Operation_Kind;
      Dest        : Operand_Kind := OD_None;
      Src         : Operand_Kind := OD_None;
      Condition   : Condition_Kind := COND_None;
      Length      : Positive := 1;
      Bit_Index   : Natural := 0;
      RST_Vector  : Word := 0) return Decoded_Instruction is
   begin
      return
        (Prefix => Main,
         Opcode => 0,
         Length => Length,
         Operation => Operation,
         Dest => Dest,
         Src => Src,
         Condition => Condition,
         Imm8 => 0,
         Imm16 => 0,
         Rel8 => 0,
         Bit_Index => Bit_Index,
         RST_Vector => RST_Vector,
         Cycles => 0,
         Jump_Cycles => 0);
   end Make;

   function Build_CB_Table return Decoded_Instruction_Table is
      Table : Decoded_Instruction_Table := [others => Make (OP_Invalid)];
      Opcode : Byte;
   begin
      for X in Half_Opcode_Field loop
         for Y in Opcode_Field loop
            for Z in Opcode_Field loop
               Opcode := Byte (Integer (X) * 64 + Integer (Y) * 8 + Integer (Z));

               case X is
                  when 0 =>
                     Table (Opcode) :=
                       Make (CB_Rotate_Operation_Map (Y),
                             Dest => Register8_Map (Z));
                  when 1 =>
                     Table (Opcode) :=
                       Make (OP_BIT,
                             Dest => OD_Bit_Index,
                             Src => Register8_Map (Z),
                             Bit_Index => Natural (Y));
                  when 2 =>
                     Table (Opcode) :=
                       Make (OP_RES,
                             Dest => Register8_Map (Z),
                             Src => Register8_Map (Z),
                             Bit_Index => Natural (Y));
                  when 3 =>
                     Table (Opcode) :=
                       Make (OP_SET,
                             Dest => Register8_Map (Z),
                             Src => Register8_Map (Z),
                             Bit_Index => Natural (Y));
               end case;
            end loop;
         end loop;
      end loop;

      return Table;
   end Build_CB_Table;

   function Build_Main_Table return Decoded_Instruction_Table is
      Table  : Decoded_Instruction_Table := [others => Make (OP_Invalid)];
      Opcode : Byte;
      X      : Half_Opcode_Field;
      Y      : Opcode_Field;
      Z      : Opcode_Field;
      P      : Half_Opcode_Field;
      Q      : Quarter_Opcode_Field;
   begin
      for Raw in 0 .. 255 loop
         Opcode := Byte (Raw);
         X := Field_X (Opcode);
         Y := Field_Y (Opcode);
         Z := Field_Z (Opcode);
         P := Field_P (Opcode);
         Q := Field_Q (Opcode);

         case X is
            when 0 =>
               case Z is
                  when 0 =>
                     case Y is
                        when 0 =>
                           Table (Opcode) := Make (OP_NOP);
                        when 1 =>
                           Table (Opcode) :=
                             Make (OP_LD, Dest => OD_Addr_Imm16, Src => OD_SP,
                                   Length => 3);
                        when 2 =>
                           Table (Opcode) := Make (OP_STOP, Length => 2);
                        when 3 =>
                           Table (Opcode) :=
                             Make (OP_JR, Src => OD_Rel8, Length => 2);
                        when others =>
                           Table (Opcode) :=
                             Make (OP_JR,
                                   Src => OD_Rel8,
                                   Condition => Condition_Map (Half_Opcode_Field (Y - 4)),
                                   Length => 2);
                     end case;
                  when 1 =>
                     if Q = 0 then
                        Table (Opcode) :=
                          Make (OP_LD,
                                Dest => Register16_Map (P),
                                Src => OD_Imm16,
                                Length => 3);
                     else
                        Table (Opcode) :=
                          Make (OP_ADD, Dest => OD_HL, Src => Register16_Map (P));
                     end if;
                  when 2 =>
                     if Q = 0 then
                        case P is
                           when 0 =>
                              Table (Opcode) := Make (OP_LD, Dest => OD_Addr_BC, Src => OD_A);
                           when 1 =>
                              Table (Opcode) := Make (OP_LD, Dest => OD_Addr_DE, Src => OD_A);
                           when 2 =>
                              Table (Opcode) := Make (OP_LD, Dest => OD_Addr_HL_Inc, Src => OD_A);
                           when 3 =>
                              Table (Opcode) := Make (OP_LD, Dest => OD_Addr_HL_Dec, Src => OD_A);
                        end case;
                     else
                        case P is
                           when 0 =>
                              Table (Opcode) := Make (OP_LD, Dest => OD_A, Src => OD_Addr_BC);
                           when 1 =>
                              Table (Opcode) := Make (OP_LD, Dest => OD_A, Src => OD_Addr_DE);
                           when 2 =>
                              Table (Opcode) := Make (OP_LD, Dest => OD_A, Src => OD_Addr_HL_Inc);
                           when 3 =>
                              Table (Opcode) := Make (OP_LD, Dest => OD_A, Src => OD_Addr_HL_Dec);
                        end case;
                     end if;
                  when 3 =>
                     if Q = 0 then
                        Table (Opcode) :=
                          Make (OP_INC, Dest => Register16_Map (P));
                     else
                        Table (Opcode) :=
                          Make (OP_DEC, Dest => Register16_Map (P));
                     end if;
                  when 4 =>
                     Table (Opcode) := Make (OP_INC, Dest => Register8_Map (Y));
                  when 5 =>
                     Table (Opcode) := Make (OP_DEC, Dest => Register8_Map (Y));
                  when 6 =>
                     Table (Opcode) :=
                       Make (OP_LD, Dest => Register8_Map (Y), Src => OD_Imm8,
                             Length => 2);
                  when 7 =>
                     case Y is
                        when 0 .. 3 =>
                           Table (Opcode) :=
                             Make (Accumulator_Rotate_Operation_Map
                                     (Half_Opcode_Field (Y)));
                        when 4 =>
                           Table (Opcode) := Make (OP_DAA);
                        when 5 =>
                           Table (Opcode) := Make (OP_CPL);
                        when 6 =>
                           Table (Opcode) := Make (OP_SCF);
                        when 7 =>
                           Table (Opcode) := Make (OP_CCF);
                     end case;
               end case;

            when 1 =>
               if Y = 6 and then Z = 6 then
                  Table (Opcode) := Make (OP_HALT);
               else
                  Table (Opcode) :=
                    Make (OP_LD, Dest => Register8_Map (Y), Src => Register8_Map (Z));
               end if;

            when 2 =>
               Table (Opcode) :=
                 Make (ALU_Operation_Map (Y), Dest => OD_A, Src => Register8_Map (Z));

            when 3 =>
               case Z is
                  when 0 =>
                     case Y is
                        when 0 .. 3 =>
                           Table (Opcode) :=
                             Make (OP_RET, Condition => Condition_Map (Half_Opcode_Field (Y)));
                        when 4 =>
                           Table (Opcode) :=
                             Make (OP_LD, Dest => OD_High_Addr_Imm8, Src => OD_A,
                                   Length => 2);
                        when 5 =>
                           Table (Opcode) :=
                             Make (OP_ADD, Dest => OD_SP, Src => OD_Rel8, Length => 2);
                        when 6 =>
                           Table (Opcode) :=
                             Make (OP_LD, Dest => OD_A, Src => OD_High_Addr_Imm8,
                                   Length => 2);
                        when 7 =>
                           Table (Opcode) :=
                             Make (OP_LD, Dest => OD_HL, Src => OD_SP_Plus_Rel8,
                                   Length => 2);
                     end case;

                  when 1 =>
                     if Q = 0 then
                        Table (Opcode) :=
                          Make (OP_POP, Dest => Register16_Stack_Map (P));
                     else
                        case P is
                           when 0 =>
                              Table (Opcode) := Make (OP_RET);
                           when 1 =>
                              Table (Opcode) := Make (OP_RETI);
                           when 2 =>
                              Table (Opcode) := Make (OP_JP, Src => OD_HL);
                           when 3 =>
                              Table (Opcode) := Make (OP_LD, Dest => OD_SP, Src => OD_HL);
                        end case;
                     end if;

                  when 2 =>
                     case Y is
                        when 0 .. 3 =>
                           Table (Opcode) :=
                             Make (OP_JP, Src => OD_Imm16,
                                   Condition => Condition_Map (Half_Opcode_Field (Y)),
                                   Length => 3);
                        when 4 =>
                           Table (Opcode) := Make (OP_LD, Dest => OD_High_Addr_C, Src => OD_A);
                        when 5 =>
                           Table (Opcode) :=
                             Make (OP_LD, Dest => OD_Addr_Imm16, Src => OD_A, Length => 3);
                        when 6 =>
                           Table (Opcode) := Make (OP_LD, Dest => OD_A, Src => OD_High_Addr_C);
                        when 7 =>
                           Table (Opcode) :=
                             Make (OP_LD, Dest => OD_A, Src => OD_Addr_Imm16, Length => 3);
                     end case;

                  when 3 =>
                     case Y is
                        when 0 =>
                           Table (Opcode) := Make (OP_JP, Src => OD_Imm16, Length => 3);
                        when 1 =>
                           Table (Opcode) := Make (OP_Invalid);
                        when 6 =>
                           Table (Opcode) := Make (OP_DI);
                        when 7 =>
                           Table (Opcode) := Make (OP_EI);
                        when others =>
                           null;
                     end case;

                  when 4 =>
                     if Y <= 3 then
                        Table (Opcode) :=
                          Make (OP_CALL, Src => OD_Imm16,
                                Condition => Condition_Map (Half_Opcode_Field (Y)),
                                Length => 3);
                     end if;

                  when 5 =>
                     if Q = 0 then
                        Table (Opcode) :=
                          Make (OP_PUSH, Src => Register16_Stack_Map (P));
                     elsif P = 0 then
                        Table (Opcode) := Make (OP_CALL, Src => OD_Imm16, Length => 3);
                     end if;

                  when 6 =>
                     Table (Opcode) :=
                       Make (ALU_Operation_Map (Y), Dest => OD_A, Src => OD_Imm8,
                             Length => 2);

                  when 7 =>
                     Table (Opcode) :=
                       Make (OP_RST, Dest => OD_RST_Vector,
                             RST_Vector => Word (Natural (Y) * 8));
               end case;
         end case;
      end loop;

      return Table;
   end Build_Main_Table;

   Main_Decode_Table : constant Decoded_Instruction_Table := Build_Main_Table;
   CB_Decode_Table   : constant Decoded_Instruction_Table := Build_CB_Table;

   function Decode
     (GB : in out Gade.GB.GB_Type) return Decoded_Instruction
   is
      Inst   : Decoded_Instruction;
      Opcode : constant Byte := Read_Byte (GB, GB.CPU.PC);
   begin
      if Opcode = 16#CB# then
         Inst := Apply_Prefix
           (CB_Decode_Table (Read_Byte (GB, GB.CPU.PC + 1)),
            Prefix => CB,
            Opcode => Read_Byte (GB, GB.CPU.PC + 1));
         Apply_Timing (Inst, Lookup_Entry (CB, Inst.Opcode));
      else
         Inst := Apply_Prefix (Main_Decode_Table (Opcode), Prefix => Main, Opcode => Opcode);

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
