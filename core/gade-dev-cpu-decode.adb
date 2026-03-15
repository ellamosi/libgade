with Gade.Dev.CPU.Instruction_Timing; use Gade.Dev.CPU.Instruction_Timing;
with Gade.GB;                         use Gade.GB;
with Gade.GB.Memory_Map;              use Gade.GB.Memory_Map;

package body Gade.Dev.CPU.Decode is

   type Decoded_Instruction_Table is array (Byte) of Decoded_Instruction;

   procedure Apply_Timing
     (Inst        : in out Decoded_Instruction;
      Table_Entry : Instruction_Timing_Entry);

   function Apply_Prefix
     (Inst   : Decoded_Instruction;
      Prefix : Prefix_Kind;
      Opcode : Byte) return Decoded_Instruction;

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
      Table_Entry : Instruction_Timing_Entry) is
   begin
      Inst.Cycles := Table_Entry.Cycles;
      Inst.Jump_Cycles := Table_Entry.Jump_Cycles;
   end Apply_Timing;

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

   Main_Decode_Table : constant Decoded_Instruction_Table :=
     [16#00# => Make (OP_NOP),
      16#01# => Make (OP_LD, Dest => OD_BC, Src => OD_Imm16, Length => 3),
      16#02# => Make (OP_LD, Dest => OD_Addr_BC, Src => OD_A),
      16#03# => Make (OP_INC, Dest => OD_BC),
      16#04# => Make (OP_INC, Dest => OD_B),
      16#05# => Make (OP_DEC, Dest => OD_B),
      16#06# => Make (OP_LD, Dest => OD_B, Src => OD_Imm8, Length => 2),
      16#07# => Make (OP_RLCA),
      16#08# => Make (OP_LD, Dest => OD_Addr_Imm16, Src => OD_SP, Length => 3),
      16#09# => Make (OP_ADD, Dest => OD_HL, Src => OD_BC),
      16#0A# => Make (OP_LD, Dest => OD_A, Src => OD_Addr_BC),
      16#0B# => Make (OP_DEC, Dest => OD_BC),
      16#0C# => Make (OP_INC, Dest => OD_C),
      16#0D# => Make (OP_DEC, Dest => OD_C),
      16#0E# => Make (OP_LD, Dest => OD_C, Src => OD_Imm8, Length => 2),
      16#0F# => Make (OP_RRCA),

      16#10# => Make (OP_STOP, Length => 2),
      16#11# => Make (OP_LD, Dest => OD_DE, Src => OD_Imm16, Length => 3),
      16#12# => Make (OP_LD, Dest => OD_Addr_DE, Src => OD_A),
      16#13# => Make (OP_INC, Dest => OD_DE),
      16#14# => Make (OP_INC, Dest => OD_D),
      16#15# => Make (OP_DEC, Dest => OD_D),
      16#16# => Make (OP_LD, Dest => OD_D, Src => OD_Imm8, Length => 2),
      16#17# => Make (OP_RLA),
      16#18# => Make (OP_JR, Src => OD_Rel8, Length => 2),
      16#19# => Make (OP_ADD, Dest => OD_HL, Src => OD_DE),
      16#1A# => Make (OP_LD, Dest => OD_A, Src => OD_Addr_DE),
      16#1B# => Make (OP_DEC, Dest => OD_DE),
      16#1C# => Make (OP_INC, Dest => OD_E),
      16#1D# => Make (OP_DEC, Dest => OD_E),
      16#1E# => Make (OP_LD, Dest => OD_E, Src => OD_Imm8, Length => 2),
      16#1F# => Make (OP_RRA),

      16#20# => Make (OP_JR, Src => OD_Rel8, Condition => COND_NZ, Length => 2),
      16#21# => Make (OP_LD, Dest => OD_HL, Src => OD_Imm16, Length => 3),
      16#22# => Make (OP_LD, Dest => OD_Addr_HL_Inc, Src => OD_A),
      16#23# => Make (OP_INC, Dest => OD_HL),
      16#24# => Make (OP_INC, Dest => OD_H),
      16#25# => Make (OP_DEC, Dest => OD_H),
      16#26# => Make (OP_LD, Dest => OD_H, Src => OD_Imm8, Length => 2),
      16#27# => Make (OP_DAA),
      16#28# => Make (OP_JR, Src => OD_Rel8, Condition => COND_Z, Length => 2),
      16#29# => Make (OP_ADD, Dest => OD_HL, Src => OD_HL),
      16#2A# => Make (OP_LD, Dest => OD_A, Src => OD_Addr_HL_Inc),
      16#2B# => Make (OP_DEC, Dest => OD_HL),
      16#2C# => Make (OP_INC, Dest => OD_L),
      16#2D# => Make (OP_DEC, Dest => OD_L),
      16#2E# => Make (OP_LD, Dest => OD_L, Src => OD_Imm8, Length => 2),
      16#2F# => Make (OP_CPL),

      16#30# => Make (OP_JR, Src => OD_Rel8, Condition => COND_NC, Length => 2),
      16#31# => Make (OP_LD, Dest => OD_SP, Src => OD_Imm16, Length => 3),
      16#32# => Make (OP_LD, Dest => OD_Addr_HL_Dec, Src => OD_A),
      16#33# => Make (OP_INC, Dest => OD_SP),
      16#34# => Make (OP_INC, Dest => OD_Addr_HL),
      16#35# => Make (OP_DEC, Dest => OD_Addr_HL),
      16#36# => Make (OP_LD, Dest => OD_Addr_HL, Src => OD_Imm8, Length => 2),
      16#37# => Make (OP_SCF),
      16#38# => Make (OP_JR, Src => OD_Rel8, Condition => COND_C, Length => 2),
      16#39# => Make (OP_ADD, Dest => OD_HL, Src => OD_SP),
      16#3A# => Make (OP_LD, Dest => OD_A, Src => OD_Addr_HL_Dec),
      16#3B# => Make (OP_DEC, Dest => OD_SP),
      16#3C# => Make (OP_INC, Dest => OD_A),
      16#3D# => Make (OP_DEC, Dest => OD_A),
      16#3E# => Make (OP_LD, Dest => OD_A, Src => OD_Imm8, Length => 2),
      16#3F# => Make (OP_CCF),

      16#40# => Make (OP_LD, Dest => OD_B, Src => OD_B),
      16#41# => Make (OP_LD, Dest => OD_B, Src => OD_C),
      16#42# => Make (OP_LD, Dest => OD_B, Src => OD_D),
      16#43# => Make (OP_LD, Dest => OD_B, Src => OD_E),
      16#44# => Make (OP_LD, Dest => OD_B, Src => OD_H),
      16#45# => Make (OP_LD, Dest => OD_B, Src => OD_L),
      16#46# => Make (OP_LD, Dest => OD_B, Src => OD_Addr_HL),
      16#47# => Make (OP_LD, Dest => OD_B, Src => OD_A),
      16#48# => Make (OP_LD, Dest => OD_C, Src => OD_B),
      16#49# => Make (OP_LD, Dest => OD_C, Src => OD_C),
      16#4A# => Make (OP_LD, Dest => OD_C, Src => OD_D),
      16#4B# => Make (OP_LD, Dest => OD_C, Src => OD_E),
      16#4C# => Make (OP_LD, Dest => OD_C, Src => OD_H),
      16#4D# => Make (OP_LD, Dest => OD_C, Src => OD_L),
      16#4E# => Make (OP_LD, Dest => OD_C, Src => OD_Addr_HL),
      16#4F# => Make (OP_LD, Dest => OD_C, Src => OD_A),

      16#50# => Make (OP_LD, Dest => OD_D, Src => OD_B),
      16#51# => Make (OP_LD, Dest => OD_D, Src => OD_C),
      16#52# => Make (OP_LD, Dest => OD_D, Src => OD_D),
      16#53# => Make (OP_LD, Dest => OD_D, Src => OD_E),
      16#54# => Make (OP_LD, Dest => OD_D, Src => OD_H),
      16#55# => Make (OP_LD, Dest => OD_D, Src => OD_L),
      16#56# => Make (OP_LD, Dest => OD_D, Src => OD_Addr_HL),
      16#57# => Make (OP_LD, Dest => OD_D, Src => OD_A),
      16#58# => Make (OP_LD, Dest => OD_E, Src => OD_B),
      16#59# => Make (OP_LD, Dest => OD_E, Src => OD_C),
      16#5A# => Make (OP_LD, Dest => OD_E, Src => OD_D),
      16#5B# => Make (OP_LD, Dest => OD_E, Src => OD_E),
      16#5C# => Make (OP_LD, Dest => OD_E, Src => OD_H),
      16#5D# => Make (OP_LD, Dest => OD_E, Src => OD_L),
      16#5E# => Make (OP_LD, Dest => OD_E, Src => OD_Addr_HL),
      16#5F# => Make (OP_LD, Dest => OD_E, Src => OD_A),

      16#60# => Make (OP_LD, Dest => OD_H, Src => OD_B),
      16#61# => Make (OP_LD, Dest => OD_H, Src => OD_C),
      16#62# => Make (OP_LD, Dest => OD_H, Src => OD_D),
      16#63# => Make (OP_LD, Dest => OD_H, Src => OD_E),
      16#64# => Make (OP_LD, Dest => OD_H, Src => OD_H),
      16#65# => Make (OP_LD, Dest => OD_H, Src => OD_L),
      16#66# => Make (OP_LD, Dest => OD_H, Src => OD_Addr_HL),
      16#67# => Make (OP_LD, Dest => OD_H, Src => OD_A),
      16#68# => Make (OP_LD, Dest => OD_L, Src => OD_B),
      16#69# => Make (OP_LD, Dest => OD_L, Src => OD_C),
      16#6A# => Make (OP_LD, Dest => OD_L, Src => OD_D),
      16#6B# => Make (OP_LD, Dest => OD_L, Src => OD_E),
      16#6C# => Make (OP_LD, Dest => OD_L, Src => OD_H),
      16#6D# => Make (OP_LD, Dest => OD_L, Src => OD_L),
      16#6E# => Make (OP_LD, Dest => OD_L, Src => OD_Addr_HL),
      16#6F# => Make (OP_LD, Dest => OD_L, Src => OD_A),

      16#70# => Make (OP_LD, Dest => OD_Addr_HL, Src => OD_B),
      16#71# => Make (OP_LD, Dest => OD_Addr_HL, Src => OD_C),
      16#72# => Make (OP_LD, Dest => OD_Addr_HL, Src => OD_D),
      16#73# => Make (OP_LD, Dest => OD_Addr_HL, Src => OD_E),
      16#74# => Make (OP_LD, Dest => OD_Addr_HL, Src => OD_H),
      16#75# => Make (OP_LD, Dest => OD_Addr_HL, Src => OD_L),
      16#76# => Make (OP_HALT),
      16#77# => Make (OP_LD, Dest => OD_Addr_HL, Src => OD_A),
      16#78# => Make (OP_LD, Dest => OD_A, Src => OD_B),
      16#79# => Make (OP_LD, Dest => OD_A, Src => OD_C),
      16#7A# => Make (OP_LD, Dest => OD_A, Src => OD_D),
      16#7B# => Make (OP_LD, Dest => OD_A, Src => OD_E),
      16#7C# => Make (OP_LD, Dest => OD_A, Src => OD_H),
      16#7D# => Make (OP_LD, Dest => OD_A, Src => OD_L),
      16#7E# => Make (OP_LD, Dest => OD_A, Src => OD_Addr_HL),
      16#7F# => Make (OP_LD, Dest => OD_A, Src => OD_A),

      16#80# => Make (OP_ADD, Dest => OD_A, Src => OD_B),
      16#81# => Make (OP_ADD, Dest => OD_A, Src => OD_C),
      16#82# => Make (OP_ADD, Dest => OD_A, Src => OD_D),
      16#83# => Make (OP_ADD, Dest => OD_A, Src => OD_E),
      16#84# => Make (OP_ADD, Dest => OD_A, Src => OD_H),
      16#85# => Make (OP_ADD, Dest => OD_A, Src => OD_L),
      16#86# => Make (OP_ADD, Dest => OD_A, Src => OD_Addr_HL),
      16#87# => Make (OP_ADD, Dest => OD_A, Src => OD_A),
      16#88# => Make (OP_ADC, Dest => OD_A, Src => OD_B),
      16#89# => Make (OP_ADC, Dest => OD_A, Src => OD_C),
      16#8A# => Make (OP_ADC, Dest => OD_A, Src => OD_D),
      16#8B# => Make (OP_ADC, Dest => OD_A, Src => OD_E),
      16#8C# => Make (OP_ADC, Dest => OD_A, Src => OD_H),
      16#8D# => Make (OP_ADC, Dest => OD_A, Src => OD_L),
      16#8E# => Make (OP_ADC, Dest => OD_A, Src => OD_Addr_HL),
      16#8F# => Make (OP_ADC, Dest => OD_A, Src => OD_A),

      16#90# => Make (OP_SUB, Dest => OD_A, Src => OD_B),
      16#91# => Make (OP_SUB, Dest => OD_A, Src => OD_C),
      16#92# => Make (OP_SUB, Dest => OD_A, Src => OD_D),
      16#93# => Make (OP_SUB, Dest => OD_A, Src => OD_E),
      16#94# => Make (OP_SUB, Dest => OD_A, Src => OD_H),
      16#95# => Make (OP_SUB, Dest => OD_A, Src => OD_L),
      16#96# => Make (OP_SUB, Dest => OD_A, Src => OD_Addr_HL),
      16#97# => Make (OP_SUB, Dest => OD_A, Src => OD_A),
      16#98# => Make (OP_SBC, Dest => OD_A, Src => OD_B),
      16#99# => Make (OP_SBC, Dest => OD_A, Src => OD_C),
      16#9A# => Make (OP_SBC, Dest => OD_A, Src => OD_D),
      16#9B# => Make (OP_SBC, Dest => OD_A, Src => OD_E),
      16#9C# => Make (OP_SBC, Dest => OD_A, Src => OD_H),
      16#9D# => Make (OP_SBC, Dest => OD_A, Src => OD_L),
      16#9E# => Make (OP_SBC, Dest => OD_A, Src => OD_Addr_HL),
      16#9F# => Make (OP_SBC, Dest => OD_A, Src => OD_A),

      16#A0# => Make (OP_AND, Dest => OD_A, Src => OD_B),
      16#A1# => Make (OP_AND, Dest => OD_A, Src => OD_C),
      16#A2# => Make (OP_AND, Dest => OD_A, Src => OD_D),
      16#A3# => Make (OP_AND, Dest => OD_A, Src => OD_E),
      16#A4# => Make (OP_AND, Dest => OD_A, Src => OD_H),
      16#A5# => Make (OP_AND, Dest => OD_A, Src => OD_L),
      16#A6# => Make (OP_AND, Dest => OD_A, Src => OD_Addr_HL),
      16#A7# => Make (OP_AND, Dest => OD_A, Src => OD_A),
      16#A8# => Make (OP_XOR, Dest => OD_A, Src => OD_B),
      16#A9# => Make (OP_XOR, Dest => OD_A, Src => OD_C),
      16#AA# => Make (OP_XOR, Dest => OD_A, Src => OD_D),
      16#AB# => Make (OP_XOR, Dest => OD_A, Src => OD_E),
      16#AC# => Make (OP_XOR, Dest => OD_A, Src => OD_H),
      16#AD# => Make (OP_XOR, Dest => OD_A, Src => OD_L),
      16#AE# => Make (OP_XOR, Dest => OD_A, Src => OD_Addr_HL),
      16#AF# => Make (OP_XOR, Dest => OD_A, Src => OD_A),

      16#B0# => Make (OP_OR, Dest => OD_A, Src => OD_B),
      16#B1# => Make (OP_OR, Dest => OD_A, Src => OD_C),
      16#B2# => Make (OP_OR, Dest => OD_A, Src => OD_D),
      16#B3# => Make (OP_OR, Dest => OD_A, Src => OD_E),
      16#B4# => Make (OP_OR, Dest => OD_A, Src => OD_H),
      16#B5# => Make (OP_OR, Dest => OD_A, Src => OD_L),
      16#B6# => Make (OP_OR, Dest => OD_A, Src => OD_Addr_HL),
      16#B7# => Make (OP_OR, Dest => OD_A, Src => OD_A),
      16#B8# => Make (OP_CP, Dest => OD_A, Src => OD_B),
      16#B9# => Make (OP_CP, Dest => OD_A, Src => OD_C),
      16#BA# => Make (OP_CP, Dest => OD_A, Src => OD_D),
      16#BB# => Make (OP_CP, Dest => OD_A, Src => OD_E),
      16#BC# => Make (OP_CP, Dest => OD_A, Src => OD_H),
      16#BD# => Make (OP_CP, Dest => OD_A, Src => OD_L),
      16#BE# => Make (OP_CP, Dest => OD_A, Src => OD_Addr_HL),
      16#BF# => Make (OP_CP, Dest => OD_A, Src => OD_A),

      16#C0# => Make (OP_RET, Condition => COND_NZ),
      16#C1# => Make (OP_POP, Dest => OD_BC),
      16#C2# => Make (OP_JP, Src => OD_Imm16, Condition => COND_NZ, Length => 3),
      16#C3# => Make (OP_JP, Src => OD_Imm16, Length => 3),
      16#C4# => Make (OP_CALL, Src => OD_Imm16, Condition => COND_NZ, Length => 3),
      16#C5# => Make (OP_PUSH, Src => OD_BC),
      16#C6# => Make (OP_ADD, Dest => OD_A, Src => OD_Imm8, Length => 2),
      16#C7# => Make (OP_RST, Dest => OD_RST_Vector, RST_Vector => 16#00#),
      16#C8# => Make (OP_RET, Condition => COND_Z),
      16#C9# => Make (OP_RET),
      16#CA# => Make (OP_JP, Src => OD_Imm16, Condition => COND_Z, Length => 3),
      16#CB# => Make (OP_Invalid),
      16#CC# => Make (OP_CALL, Src => OD_Imm16, Condition => COND_Z, Length => 3),
      16#CD# => Make (OP_CALL, Src => OD_Imm16, Length => 3),
      16#CE# => Make (OP_ADC, Dest => OD_A, Src => OD_Imm8, Length => 2),
      16#CF# => Make (OP_RST, Dest => OD_RST_Vector, RST_Vector => 16#08#),

      16#D0# => Make (OP_RET, Condition => COND_NC),
      16#D1# => Make (OP_POP, Dest => OD_DE),
      16#D2# => Make (OP_JP, Src => OD_Imm16, Condition => COND_NC, Length => 3),
      16#D3# => Make (OP_Invalid),
      16#D4# => Make (OP_CALL, Src => OD_Imm16, Condition => COND_NC, Length => 3),
      16#D5# => Make (OP_PUSH, Src => OD_DE),
      16#D6# => Make (OP_SUB, Dest => OD_A, Src => OD_Imm8, Length => 2),
      16#D7# => Make (OP_RST, Dest => OD_RST_Vector, RST_Vector => 16#10#),
      16#D8# => Make (OP_RET, Condition => COND_C),
      16#D9# => Make (OP_RETI),
      16#DA# => Make (OP_JP, Src => OD_Imm16, Condition => COND_C, Length => 3),
      16#DB# => Make (OP_Invalid),
      16#DC# => Make (OP_CALL, Src => OD_Imm16, Condition => COND_C, Length => 3),
      16#DD# => Make (OP_Invalid),
      16#DE# => Make (OP_SBC, Dest => OD_A, Src => OD_Imm8, Length => 2),
      16#DF# => Make (OP_RST, Dest => OD_RST_Vector, RST_Vector => 16#18#),

      16#E0# => Make (OP_LD, Dest => OD_High_Addr_Imm8, Src => OD_A, Length => 2),
      16#E1# => Make (OP_POP, Dest => OD_HL),
      16#E2# => Make (OP_LD, Dest => OD_High_Addr_C, Src => OD_A),
      16#E3# => Make (OP_Invalid),
      16#E4# => Make (OP_Invalid),
      16#E5# => Make (OP_PUSH, Src => OD_HL),
      16#E6# => Make (OP_AND, Dest => OD_A, Src => OD_Imm8, Length => 2),
      16#E7# => Make (OP_RST, Dest => OD_RST_Vector, RST_Vector => 16#20#),
      16#E8# => Make (OP_ADD, Dest => OD_SP, Src => OD_Rel8, Length => 2),
      16#E9# => Make (OP_JP, Src => OD_HL),
      16#EA# => Make (OP_LD, Dest => OD_Addr_Imm16, Src => OD_A, Length => 3),
      16#EB# => Make (OP_Invalid),
      16#EC# => Make (OP_Invalid),
      16#ED# => Make (OP_Invalid),
      16#EE# => Make (OP_XOR, Dest => OD_A, Src => OD_Imm8, Length => 2),
      16#EF# => Make (OP_RST, Dest => OD_RST_Vector, RST_Vector => 16#28#),

      16#F0# => Make (OP_LD, Dest => OD_A, Src => OD_High_Addr_Imm8, Length => 2),
      16#F1# => Make (OP_POP, Dest => OD_AF),
      16#F2# => Make (OP_LD, Dest => OD_A, Src => OD_High_Addr_C),
      16#F3# => Make (OP_DI),
      16#F4# => Make (OP_Invalid),
      16#F5# => Make (OP_PUSH, Src => OD_AF),
      16#F6# => Make (OP_OR, Dest => OD_A, Src => OD_Imm8, Length => 2),
      16#F7# => Make (OP_RST, Dest => OD_RST_Vector, RST_Vector => 16#30#),
      16#F8# => Make (OP_LD, Dest => OD_HL, Src => OD_SP_Plus_Rel8, Length => 2),
      16#F9# => Make (OP_LD, Dest => OD_SP, Src => OD_HL),
      16#FA# => Make (OP_LD, Dest => OD_A, Src => OD_Addr_Imm16, Length => 3),
      16#FB# => Make (OP_EI),
      16#FC# => Make (OP_Invalid),
      16#FD# => Make (OP_Invalid),
      16#FE# => Make (OP_CP, Dest => OD_A, Src => OD_Imm8, Length => 2),
      16#FF# => Make (OP_RST, Dest => OD_RST_Vector, RST_Vector => 16#38#)];

   CB_Decode_Table : constant Decoded_Instruction_Table :=
     [16#00# => Make (OP_RLC, Dest => OD_B),
      16#01# => Make (OP_RLC, Dest => OD_C),
      16#02# => Make (OP_RLC, Dest => OD_D),
      16#03# => Make (OP_RLC, Dest => OD_E),
      16#04# => Make (OP_RLC, Dest => OD_H),
      16#05# => Make (OP_RLC, Dest => OD_L),
      16#06# => Make (OP_RLC, Dest => OD_Addr_HL),
      16#07# => Make (OP_RLC, Dest => OD_A),
      16#08# => Make (OP_RRC, Dest => OD_B),
      16#09# => Make (OP_RRC, Dest => OD_C),
      16#0A# => Make (OP_RRC, Dest => OD_D),
      16#0B# => Make (OP_RRC, Dest => OD_E),
      16#0C# => Make (OP_RRC, Dest => OD_H),
      16#0D# => Make (OP_RRC, Dest => OD_L),
      16#0E# => Make (OP_RRC, Dest => OD_Addr_HL),
      16#0F# => Make (OP_RRC, Dest => OD_A),

      16#10# => Make (OP_RL, Dest => OD_B),
      16#11# => Make (OP_RL, Dest => OD_C),
      16#12# => Make (OP_RL, Dest => OD_D),
      16#13# => Make (OP_RL, Dest => OD_E),
      16#14# => Make (OP_RL, Dest => OD_H),
      16#15# => Make (OP_RL, Dest => OD_L),
      16#16# => Make (OP_RL, Dest => OD_Addr_HL),
      16#17# => Make (OP_RL, Dest => OD_A),
      16#18# => Make (OP_RR, Dest => OD_B),
      16#19# => Make (OP_RR, Dest => OD_C),
      16#1A# => Make (OP_RR, Dest => OD_D),
      16#1B# => Make (OP_RR, Dest => OD_E),
      16#1C# => Make (OP_RR, Dest => OD_H),
      16#1D# => Make (OP_RR, Dest => OD_L),
      16#1E# => Make (OP_RR, Dest => OD_Addr_HL),
      16#1F# => Make (OP_RR, Dest => OD_A),

      16#20# => Make (OP_SLA, Dest => OD_B),
      16#21# => Make (OP_SLA, Dest => OD_C),
      16#22# => Make (OP_SLA, Dest => OD_D),
      16#23# => Make (OP_SLA, Dest => OD_E),
      16#24# => Make (OP_SLA, Dest => OD_H),
      16#25# => Make (OP_SLA, Dest => OD_L),
      16#26# => Make (OP_SLA, Dest => OD_Addr_HL),
      16#27# => Make (OP_SLA, Dest => OD_A),
      16#28# => Make (OP_SRA, Dest => OD_B),
      16#29# => Make (OP_SRA, Dest => OD_C),
      16#2A# => Make (OP_SRA, Dest => OD_D),
      16#2B# => Make (OP_SRA, Dest => OD_E),
      16#2C# => Make (OP_SRA, Dest => OD_H),
      16#2D# => Make (OP_SRA, Dest => OD_L),
      16#2E# => Make (OP_SRA, Dest => OD_Addr_HL),
      16#2F# => Make (OP_SRA, Dest => OD_A),

      16#30# => Make (OP_SWAP, Dest => OD_B),
      16#31# => Make (OP_SWAP, Dest => OD_C),
      16#32# => Make (OP_SWAP, Dest => OD_D),
      16#33# => Make (OP_SWAP, Dest => OD_E),
      16#34# => Make (OP_SWAP, Dest => OD_H),
      16#35# => Make (OP_SWAP, Dest => OD_L),
      16#36# => Make (OP_SWAP, Dest => OD_Addr_HL),
      16#37# => Make (OP_SWAP, Dest => OD_A),
      16#38# => Make (OP_SRL, Dest => OD_B),
      16#39# => Make (OP_SRL, Dest => OD_C),
      16#3A# => Make (OP_SRL, Dest => OD_D),
      16#3B# => Make (OP_SRL, Dest => OD_E),
      16#3C# => Make (OP_SRL, Dest => OD_H),
      16#3D# => Make (OP_SRL, Dest => OD_L),
      16#3E# => Make (OP_SRL, Dest => OD_Addr_HL),
      16#3F# => Make (OP_SRL, Dest => OD_A),

      16#40# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_B, Bit_Index => 0),
      16#41# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_C, Bit_Index => 0),
      16#42# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_D, Bit_Index => 0),
      16#43# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_E, Bit_Index => 0),
      16#44# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_H, Bit_Index => 0),
      16#45# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_L, Bit_Index => 0),
      16#46# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_Addr_HL, Bit_Index => 0),
      16#47# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_A, Bit_Index => 0),
      16#48# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_B, Bit_Index => 1),
      16#49# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_C, Bit_Index => 1),
      16#4A# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_D, Bit_Index => 1),
      16#4B# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_E, Bit_Index => 1),
      16#4C# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_H, Bit_Index => 1),
      16#4D# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_L, Bit_Index => 1),
      16#4E# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_Addr_HL, Bit_Index => 1),
      16#4F# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_A, Bit_Index => 1),

      16#50# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_B, Bit_Index => 2),
      16#51# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_C, Bit_Index => 2),
      16#52# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_D, Bit_Index => 2),
      16#53# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_E, Bit_Index => 2),
      16#54# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_H, Bit_Index => 2),
      16#55# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_L, Bit_Index => 2),
      16#56# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_Addr_HL, Bit_Index => 2),
      16#57# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_A, Bit_Index => 2),
      16#58# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_B, Bit_Index => 3),
      16#59# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_C, Bit_Index => 3),
      16#5A# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_D, Bit_Index => 3),
      16#5B# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_E, Bit_Index => 3),
      16#5C# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_H, Bit_Index => 3),
      16#5D# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_L, Bit_Index => 3),
      16#5E# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_Addr_HL, Bit_Index => 3),
      16#5F# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_A, Bit_Index => 3),

      16#60# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_B, Bit_Index => 4),
      16#61# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_C, Bit_Index => 4),
      16#62# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_D, Bit_Index => 4),
      16#63# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_E, Bit_Index => 4),
      16#64# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_H, Bit_Index => 4),
      16#65# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_L, Bit_Index => 4),
      16#66# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_Addr_HL, Bit_Index => 4),
      16#67# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_A, Bit_Index => 4),
      16#68# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_B, Bit_Index => 5),
      16#69# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_C, Bit_Index => 5),
      16#6A# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_D, Bit_Index => 5),
      16#6B# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_E, Bit_Index => 5),
      16#6C# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_H, Bit_Index => 5),
      16#6D# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_L, Bit_Index => 5),
      16#6E# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_Addr_HL, Bit_Index => 5),
      16#6F# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_A, Bit_Index => 5),

      16#70# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_B, Bit_Index => 6),
      16#71# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_C, Bit_Index => 6),
      16#72# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_D, Bit_Index => 6),
      16#73# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_E, Bit_Index => 6),
      16#74# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_H, Bit_Index => 6),
      16#75# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_L, Bit_Index => 6),
      16#76# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_Addr_HL, Bit_Index => 6),
      16#77# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_A, Bit_Index => 6),
      16#78# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_B, Bit_Index => 7),
      16#79# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_C, Bit_Index => 7),
      16#7A# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_D, Bit_Index => 7),
      16#7B# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_E, Bit_Index => 7),
      16#7C# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_H, Bit_Index => 7),
      16#7D# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_L, Bit_Index => 7),
      16#7E# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_Addr_HL, Bit_Index => 7),
      16#7F# => Make (OP_BIT, Dest => OD_Bit_Index, Src => OD_A, Bit_Index => 7),

      16#80# => Make (OP_RES, Dest => OD_B, Src => OD_B, Bit_Index => 0),
      16#81# => Make (OP_RES, Dest => OD_C, Src => OD_C, Bit_Index => 0),
      16#82# => Make (OP_RES, Dest => OD_D, Src => OD_D, Bit_Index => 0),
      16#83# => Make (OP_RES, Dest => OD_E, Src => OD_E, Bit_Index => 0),
      16#84# => Make (OP_RES, Dest => OD_H, Src => OD_H, Bit_Index => 0),
      16#85# => Make (OP_RES, Dest => OD_L, Src => OD_L, Bit_Index => 0),
      16#86# => Make (OP_RES, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 0),
      16#87# => Make (OP_RES, Dest => OD_A, Src => OD_A, Bit_Index => 0),
      16#88# => Make (OP_RES, Dest => OD_B, Src => OD_B, Bit_Index => 1),
      16#89# => Make (OP_RES, Dest => OD_C, Src => OD_C, Bit_Index => 1),
      16#8A# => Make (OP_RES, Dest => OD_D, Src => OD_D, Bit_Index => 1),
      16#8B# => Make (OP_RES, Dest => OD_E, Src => OD_E, Bit_Index => 1),
      16#8C# => Make (OP_RES, Dest => OD_H, Src => OD_H, Bit_Index => 1),
      16#8D# => Make (OP_RES, Dest => OD_L, Src => OD_L, Bit_Index => 1),
      16#8E# => Make (OP_RES, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 1),
      16#8F# => Make (OP_RES, Dest => OD_A, Src => OD_A, Bit_Index => 1),

      16#90# => Make (OP_RES, Dest => OD_B, Src => OD_B, Bit_Index => 2),
      16#91# => Make (OP_RES, Dest => OD_C, Src => OD_C, Bit_Index => 2),
      16#92# => Make (OP_RES, Dest => OD_D, Src => OD_D, Bit_Index => 2),
      16#93# => Make (OP_RES, Dest => OD_E, Src => OD_E, Bit_Index => 2),
      16#94# => Make (OP_RES, Dest => OD_H, Src => OD_H, Bit_Index => 2),
      16#95# => Make (OP_RES, Dest => OD_L, Src => OD_L, Bit_Index => 2),
      16#96# => Make (OP_RES, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 2),
      16#97# => Make (OP_RES, Dest => OD_A, Src => OD_A, Bit_Index => 2),
      16#98# => Make (OP_RES, Dest => OD_B, Src => OD_B, Bit_Index => 3),
      16#99# => Make (OP_RES, Dest => OD_C, Src => OD_C, Bit_Index => 3),
      16#9A# => Make (OP_RES, Dest => OD_D, Src => OD_D, Bit_Index => 3),
      16#9B# => Make (OP_RES, Dest => OD_E, Src => OD_E, Bit_Index => 3),
      16#9C# => Make (OP_RES, Dest => OD_H, Src => OD_H, Bit_Index => 3),
      16#9D# => Make (OP_RES, Dest => OD_L, Src => OD_L, Bit_Index => 3),
      16#9E# => Make (OP_RES, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 3),
      16#9F# => Make (OP_RES, Dest => OD_A, Src => OD_A, Bit_Index => 3),

      16#A0# => Make (OP_RES, Dest => OD_B, Src => OD_B, Bit_Index => 4),
      16#A1# => Make (OP_RES, Dest => OD_C, Src => OD_C, Bit_Index => 4),
      16#A2# => Make (OP_RES, Dest => OD_D, Src => OD_D, Bit_Index => 4),
      16#A3# => Make (OP_RES, Dest => OD_E, Src => OD_E, Bit_Index => 4),
      16#A4# => Make (OP_RES, Dest => OD_H, Src => OD_H, Bit_Index => 4),
      16#A5# => Make (OP_RES, Dest => OD_L, Src => OD_L, Bit_Index => 4),
      16#A6# => Make (OP_RES, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 4),
      16#A7# => Make (OP_RES, Dest => OD_A, Src => OD_A, Bit_Index => 4),
      16#A8# => Make (OP_RES, Dest => OD_B, Src => OD_B, Bit_Index => 5),
      16#A9# => Make (OP_RES, Dest => OD_C, Src => OD_C, Bit_Index => 5),
      16#AA# => Make (OP_RES, Dest => OD_D, Src => OD_D, Bit_Index => 5),
      16#AB# => Make (OP_RES, Dest => OD_E, Src => OD_E, Bit_Index => 5),
      16#AC# => Make (OP_RES, Dest => OD_H, Src => OD_H, Bit_Index => 5),
      16#AD# => Make (OP_RES, Dest => OD_L, Src => OD_L, Bit_Index => 5),
      16#AE# => Make (OP_RES, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 5),
      16#AF# => Make (OP_RES, Dest => OD_A, Src => OD_A, Bit_Index => 5),

      16#B0# => Make (OP_RES, Dest => OD_B, Src => OD_B, Bit_Index => 6),
      16#B1# => Make (OP_RES, Dest => OD_C, Src => OD_C, Bit_Index => 6),
      16#B2# => Make (OP_RES, Dest => OD_D, Src => OD_D, Bit_Index => 6),
      16#B3# => Make (OP_RES, Dest => OD_E, Src => OD_E, Bit_Index => 6),
      16#B4# => Make (OP_RES, Dest => OD_H, Src => OD_H, Bit_Index => 6),
      16#B5# => Make (OP_RES, Dest => OD_L, Src => OD_L, Bit_Index => 6),
      16#B6# => Make (OP_RES, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 6),
      16#B7# => Make (OP_RES, Dest => OD_A, Src => OD_A, Bit_Index => 6),
      16#B8# => Make (OP_RES, Dest => OD_B, Src => OD_B, Bit_Index => 7),
      16#B9# => Make (OP_RES, Dest => OD_C, Src => OD_C, Bit_Index => 7),
      16#BA# => Make (OP_RES, Dest => OD_D, Src => OD_D, Bit_Index => 7),
      16#BB# => Make (OP_RES, Dest => OD_E, Src => OD_E, Bit_Index => 7),
      16#BC# => Make (OP_RES, Dest => OD_H, Src => OD_H, Bit_Index => 7),
      16#BD# => Make (OP_RES, Dest => OD_L, Src => OD_L, Bit_Index => 7),
      16#BE# => Make (OP_RES, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 7),
      16#BF# => Make (OP_RES, Dest => OD_A, Src => OD_A, Bit_Index => 7),

      16#C0# => Make (OP_SET, Dest => OD_B, Src => OD_B, Bit_Index => 0),
      16#C1# => Make (OP_SET, Dest => OD_C, Src => OD_C, Bit_Index => 0),
      16#C2# => Make (OP_SET, Dest => OD_D, Src => OD_D, Bit_Index => 0),
      16#C3# => Make (OP_SET, Dest => OD_E, Src => OD_E, Bit_Index => 0),
      16#C4# => Make (OP_SET, Dest => OD_H, Src => OD_H, Bit_Index => 0),
      16#C5# => Make (OP_SET, Dest => OD_L, Src => OD_L, Bit_Index => 0),
      16#C6# => Make (OP_SET, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 0),
      16#C7# => Make (OP_SET, Dest => OD_A, Src => OD_A, Bit_Index => 0),
      16#C8# => Make (OP_SET, Dest => OD_B, Src => OD_B, Bit_Index => 1),
      16#C9# => Make (OP_SET, Dest => OD_C, Src => OD_C, Bit_Index => 1),
      16#CA# => Make (OP_SET, Dest => OD_D, Src => OD_D, Bit_Index => 1),
      16#CB# => Make (OP_SET, Dest => OD_E, Src => OD_E, Bit_Index => 1),
      16#CC# => Make (OP_SET, Dest => OD_H, Src => OD_H, Bit_Index => 1),
      16#CD# => Make (OP_SET, Dest => OD_L, Src => OD_L, Bit_Index => 1),
      16#CE# => Make (OP_SET, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 1),
      16#CF# => Make (OP_SET, Dest => OD_A, Src => OD_A, Bit_Index => 1),

      16#D0# => Make (OP_SET, Dest => OD_B, Src => OD_B, Bit_Index => 2),
      16#D1# => Make (OP_SET, Dest => OD_C, Src => OD_C, Bit_Index => 2),
      16#D2# => Make (OP_SET, Dest => OD_D, Src => OD_D, Bit_Index => 2),
      16#D3# => Make (OP_SET, Dest => OD_E, Src => OD_E, Bit_Index => 2),
      16#D4# => Make (OP_SET, Dest => OD_H, Src => OD_H, Bit_Index => 2),
      16#D5# => Make (OP_SET, Dest => OD_L, Src => OD_L, Bit_Index => 2),
      16#D6# => Make (OP_SET, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 2),
      16#D7# => Make (OP_SET, Dest => OD_A, Src => OD_A, Bit_Index => 2),
      16#D8# => Make (OP_SET, Dest => OD_B, Src => OD_B, Bit_Index => 3),
      16#D9# => Make (OP_SET, Dest => OD_C, Src => OD_C, Bit_Index => 3),
      16#DA# => Make (OP_SET, Dest => OD_D, Src => OD_D, Bit_Index => 3),
      16#DB# => Make (OP_SET, Dest => OD_E, Src => OD_E, Bit_Index => 3),
      16#DC# => Make (OP_SET, Dest => OD_H, Src => OD_H, Bit_Index => 3),
      16#DD# => Make (OP_SET, Dest => OD_L, Src => OD_L, Bit_Index => 3),
      16#DE# => Make (OP_SET, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 3),
      16#DF# => Make (OP_SET, Dest => OD_A, Src => OD_A, Bit_Index => 3),

      16#E0# => Make (OP_SET, Dest => OD_B, Src => OD_B, Bit_Index => 4),
      16#E1# => Make (OP_SET, Dest => OD_C, Src => OD_C, Bit_Index => 4),
      16#E2# => Make (OP_SET, Dest => OD_D, Src => OD_D, Bit_Index => 4),
      16#E3# => Make (OP_SET, Dest => OD_E, Src => OD_E, Bit_Index => 4),
      16#E4# => Make (OP_SET, Dest => OD_H, Src => OD_H, Bit_Index => 4),
      16#E5# => Make (OP_SET, Dest => OD_L, Src => OD_L, Bit_Index => 4),
      16#E6# => Make (OP_SET, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 4),
      16#E7# => Make (OP_SET, Dest => OD_A, Src => OD_A, Bit_Index => 4),
      16#E8# => Make (OP_SET, Dest => OD_B, Src => OD_B, Bit_Index => 5),
      16#E9# => Make (OP_SET, Dest => OD_C, Src => OD_C, Bit_Index => 5),
      16#EA# => Make (OP_SET, Dest => OD_D, Src => OD_D, Bit_Index => 5),
      16#EB# => Make (OP_SET, Dest => OD_E, Src => OD_E, Bit_Index => 5),
      16#EC# => Make (OP_SET, Dest => OD_H, Src => OD_H, Bit_Index => 5),
      16#ED# => Make (OP_SET, Dest => OD_L, Src => OD_L, Bit_Index => 5),
      16#EE# => Make (OP_SET, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 5),
      16#EF# => Make (OP_SET, Dest => OD_A, Src => OD_A, Bit_Index => 5),

      16#F0# => Make (OP_SET, Dest => OD_B, Src => OD_B, Bit_Index => 6),
      16#F1# => Make (OP_SET, Dest => OD_C, Src => OD_C, Bit_Index => 6),
      16#F2# => Make (OP_SET, Dest => OD_D, Src => OD_D, Bit_Index => 6),
      16#F3# => Make (OP_SET, Dest => OD_E, Src => OD_E, Bit_Index => 6),
      16#F4# => Make (OP_SET, Dest => OD_H, Src => OD_H, Bit_Index => 6),
      16#F5# => Make (OP_SET, Dest => OD_L, Src => OD_L, Bit_Index => 6),
      16#F6# => Make (OP_SET, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 6),
      16#F7# => Make (OP_SET, Dest => OD_A, Src => OD_A, Bit_Index => 6),
      16#F8# => Make (OP_SET, Dest => OD_B, Src => OD_B, Bit_Index => 7),
      16#F9# => Make (OP_SET, Dest => OD_C, Src => OD_C, Bit_Index => 7),
      16#FA# => Make (OP_SET, Dest => OD_D, Src => OD_D, Bit_Index => 7),
      16#FB# => Make (OP_SET, Dest => OD_E, Src => OD_E, Bit_Index => 7),
      16#FC# => Make (OP_SET, Dest => OD_H, Src => OD_H, Bit_Index => 7),
      16#FD# => Make (OP_SET, Dest => OD_L, Src => OD_L, Bit_Index => 7),
      16#FE# => Make (OP_SET, Dest => OD_Addr_HL, Src => OD_Addr_HL, Bit_Index => 7),
      16#FF# => Make (OP_SET, Dest => OD_A, Src => OD_A, Bit_Index => 7)];

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
         Inst.Length := 2;
         Apply_Timing (Inst, Opcodes_CB (Inst.Opcode));
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

         Apply_Timing (Inst, Opcodes_Main (Opcode));
      end if;

      return Inst;
   end Decode;

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
