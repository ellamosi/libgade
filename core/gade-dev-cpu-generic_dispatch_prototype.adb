with Gade.Dev.CPU.Arithmetic; use Gade.Dev.CPU.Arithmetic;
with Gade.Dev.CPU.Bitwise;    use Gade.Dev.CPU.Bitwise;
with Gade.Dev.CPU.Decode;     use Gade.Dev.CPU.Decode;
with Gade.Dev.CPU.Generic_Instruction_Definitions;
with Gade.Dev.CPU.Logic;      use Gade.Dev.CPU.Logic;
with Gade.GB.Memory_Map;

package body Gade.Dev.CPU.Generic_Dispatch_Prototype is
   package Definitions renames Gade.Dev.CPU.Generic_Instruction_Definitions;

   type Handler_Table is array (Byte) of Instruction_Handler;

   function Build_Main_Table return Handler_Table;

   function Build_CB_Table return Handler_Table;

   function Read_Byte_Operand
     (GB      : in out Gade.GB.GB_Type;
      Operand :        Operand_Kind;
      Inst    :        Decoded_Instruction) return Byte;

   procedure Write_Byte_Operand
     (GB      : in out Gade.GB.GB_Type;
      Operand :        Operand_Kind;
      Value   :        Byte);

   function Build_Main_Table return Handler_Table is
      Table : Handler_Table := [others => null];
   begin
      Table (16#80#) := Definitions.Execute_ADD_A_B'Access;
      Table (16#86#) := Definitions.Execute_ADD_A_Addr_HL'Access;
      Table (16#89#) := Definitions.Execute_ADC_A_C'Access;
      Table (16#92#) := Definitions.Execute_SUB_A_D'Access;
      Table (16#9B#) := Definitions.Execute_SBC_A_E'Access;
      Table (16#A4#) := Definitions.Execute_AND_A_H'Access;
      Table (16#AD#) := Definitions.Execute_XOR_A_L'Access;
      Table (16#B7#) := Definitions.Execute_OR_A_A'Access;
      Table (16#BE#) := Definitions.Execute_CP_A_Addr_HL'Access;
      Table (16#01#) := Definitions.Execute_LD_BC_Imm16'Access;
      Table (16#02#) := Definitions.Execute_LD_Addr_BC_A'Access;
      Table (16#06#) := Definitions.Execute_LD_B_Imm8'Access;
      Table (16#08#) := Definitions.Execute_LD_Addr_Imm16_SP'Access;
      Table (16#0A#) := Definitions.Execute_LD_A_Addr_BC'Access;
      Table (16#0E#) := Definitions.Execute_LD_C_Imm8'Access;
      Table (16#11#) := Definitions.Execute_LD_DE_Imm16'Access;
      Table (16#12#) := Definitions.Execute_LD_Addr_DE_A'Access;
      Table (16#16#) := Definitions.Execute_LD_D_Imm8'Access;
      Table (16#1A#) := Definitions.Execute_LD_A_Addr_DE'Access;
      Table (16#1E#) := Definitions.Execute_LD_E_Imm8'Access;
      Table (16#21#) := Definitions.Execute_LD_HL_Imm16'Access;
      Table (16#22#) := Definitions.Execute_LD_Addr_HL_Inc_A'Access;
      Table (16#26#) := Definitions.Execute_LD_H_Imm8'Access;
      Table (16#2A#) := Definitions.Execute_LD_A_Addr_HL_Inc'Access;
      Table (16#2E#) := Definitions.Execute_LD_L_Imm8'Access;
      Table (16#31#) := Definitions.Execute_LD_SP_Imm16'Access;
      Table (16#32#) := Definitions.Execute_LD_Addr_HL_Dec_A'Access;
      Table (16#36#) := Definitions.Execute_LD_Addr_HL_Imm8'Access;
      Table (16#3A#) := Definitions.Execute_LD_A_Addr_HL_Dec'Access;
      Table (16#3E#) := Definitions.Execute_LD_A_Imm8'Access;
      Table (16#40#) := Definitions.Execute_LD_B_B'Access;
      Table (16#41#) := Definitions.Execute_LD_B_C'Access;
      Table (16#42#) := Definitions.Execute_LD_B_D'Access;
      Table (16#43#) := Definitions.Execute_LD_B_E'Access;
      Table (16#44#) := Definitions.Execute_LD_B_H'Access;
      Table (16#45#) := Definitions.Execute_LD_B_L'Access;
      Table (16#46#) := Definitions.Execute_LD_B_Addr_HL'Access;
      Table (16#47#) := Definitions.Execute_LD_B_A'Access;
      Table (16#48#) := Definitions.Execute_LD_C_B'Access;
      Table (16#49#) := Definitions.Execute_LD_C_C'Access;
      Table (16#4A#) := Definitions.Execute_LD_C_D'Access;
      Table (16#4B#) := Definitions.Execute_LD_C_E'Access;
      Table (16#4C#) := Definitions.Execute_LD_C_H'Access;
      Table (16#4D#) := Definitions.Execute_LD_C_L'Access;
      Table (16#4E#) := Definitions.Execute_LD_C_Addr_HL'Access;
      Table (16#4F#) := Definitions.Execute_LD_C_A'Access;
      Table (16#50#) := Definitions.Execute_LD_D_B'Access;
      Table (16#51#) := Definitions.Execute_LD_D_C'Access;
      Table (16#52#) := Definitions.Execute_LD_D_D'Access;
      Table (16#53#) := Definitions.Execute_LD_D_E'Access;
      Table (16#54#) := Definitions.Execute_LD_D_H'Access;
      Table (16#55#) := Definitions.Execute_LD_D_L'Access;
      Table (16#56#) := Definitions.Execute_LD_D_Addr_HL'Access;
      Table (16#57#) := Definitions.Execute_LD_D_A'Access;
      Table (16#58#) := Definitions.Execute_LD_E_B'Access;
      Table (16#59#) := Definitions.Execute_LD_E_C'Access;
      Table (16#5A#) := Definitions.Execute_LD_E_D'Access;
      Table (16#5B#) := Definitions.Execute_LD_E_E'Access;
      Table (16#5C#) := Definitions.Execute_LD_E_H'Access;
      Table (16#5D#) := Definitions.Execute_LD_E_L'Access;
      Table (16#5E#) := Definitions.Execute_LD_E_Addr_HL'Access;
      Table (16#5F#) := Definitions.Execute_LD_E_A'Access;
      Table (16#60#) := Definitions.Execute_LD_H_B'Access;
      Table (16#61#) := Definitions.Execute_LD_H_C'Access;
      Table (16#62#) := Definitions.Execute_LD_H_D'Access;
      Table (16#63#) := Definitions.Execute_LD_H_E'Access;
      Table (16#64#) := Definitions.Execute_LD_H_H'Access;
      Table (16#65#) := Definitions.Execute_LD_H_L'Access;
      Table (16#66#) := Definitions.Execute_LD_H_Addr_HL'Access;
      Table (16#67#) := Definitions.Execute_LD_H_A'Access;
      Table (16#68#) := Definitions.Execute_LD_L_B'Access;
      Table (16#69#) := Definitions.Execute_LD_L_C'Access;
      Table (16#6A#) := Definitions.Execute_LD_L_D'Access;
      Table (16#6B#) := Definitions.Execute_LD_L_E'Access;
      Table (16#6C#) := Definitions.Execute_LD_L_H'Access;
      Table (16#6D#) := Definitions.Execute_LD_L_L'Access;
      Table (16#6E#) := Definitions.Execute_LD_L_Addr_HL'Access;
      Table (16#6F#) := Definitions.Execute_LD_L_A'Access;
      Table (16#70#) := Definitions.Execute_LD_Addr_HL_B'Access;
      Table (16#71#) := Definitions.Execute_LD_Addr_HL_C'Access;
      Table (16#72#) := Definitions.Execute_LD_Addr_HL_D'Access;
      Table (16#73#) := Definitions.Execute_LD_Addr_HL_E'Access;
      Table (16#74#) := Definitions.Execute_LD_Addr_HL_H'Access;
      Table (16#75#) := Definitions.Execute_LD_Addr_HL_L'Access;
      Table (16#77#) := Definitions.Execute_LD_Addr_HL_A'Access;
      Table (16#78#) := Definitions.Execute_LD_A_B'Access;
      Table (16#79#) := Definitions.Execute_LD_A_C'Access;
      Table (16#7A#) := Definitions.Execute_LD_A_D'Access;
      Table (16#7B#) := Definitions.Execute_LD_A_E'Access;
      Table (16#7C#) := Definitions.Execute_LD_A_H'Access;
      Table (16#7D#) := Definitions.Execute_LD_A_L'Access;
      Table (16#7E#) := Definitions.Execute_LD_A_Addr_HL'Access;
      Table (16#7F#) := Definitions.Execute_LD_A_A'Access;
      Table (16#3E#) := Definitions.Execute_LD_A_Imm8'Access;
      Table (16#36#) := Definitions.Execute_LD_Addr_HL_Imm8'Access;
      Table (16#03#) := Definitions.Execute_INC_BC'Access;
      Table (16#04#) := Definitions.Execute_INC_B'Access;
      Table (16#05#) := Definitions.Execute_DEC_B'Access;
      Table (16#0B#) := Definitions.Execute_DEC_BC'Access;
      Table (16#0C#) := Definitions.Execute_INC_C'Access;
      Table (16#0D#) := Definitions.Execute_DEC_C'Access;
      Table (16#13#) := Definitions.Execute_INC_DE'Access;
      Table (16#14#) := Definitions.Execute_INC_D'Access;
      Table (16#15#) := Definitions.Execute_DEC_D'Access;
      Table (16#1B#) := Definitions.Execute_DEC_DE'Access;
      Table (16#1C#) := Definitions.Execute_INC_E'Access;
      Table (16#1D#) := Definitions.Execute_DEC_E'Access;
      Table (16#35#) := Definitions.Execute_DEC_Addr_HL'Access;
      Table (16#23#) := Definitions.Execute_INC_HL'Access;
      Table (16#24#) := Definitions.Execute_INC_H'Access;
      Table (16#25#) := Definitions.Execute_DEC_H'Access;
      Table (16#2B#) := Definitions.Execute_DEC_HL'Access;
      Table (16#2C#) := Definitions.Execute_INC_L'Access;
      Table (16#2D#) := Definitions.Execute_DEC_L'Access;
      Table (16#33#) := Definitions.Execute_INC_SP'Access;
      Table (16#34#) := Definitions.Execute_INC_Addr_HL'Access;
      Table (16#3B#) := Definitions.Execute_DEC_SP'Access;
      Table (16#3C#) := Definitions.Execute_INC_A'Access;
      Table (16#3D#) := Definitions.Execute_DEC_A'Access;
      Table (16#07#) := Definitions.Execute_RLCA'Access;
      Table (16#0F#) := Definitions.Execute_RRCA'Access;
      Table (16#17#) := Definitions.Execute_RLA'Access;
      Table (16#1F#) := Definitions.Execute_RRA'Access;
      Table (16#C1#) := Definitions.Execute_POP_BC'Access;
      Table (16#C5#) := Definitions.Execute_PUSH_BC'Access;
      Table (16#D1#) := Definitions.Execute_POP_DE'Access;
      Table (16#D5#) := Definitions.Execute_PUSH_DE'Access;
      Table (16#E1#) := Definitions.Execute_POP_HL'Access;
      Table (16#E5#) := Definitions.Execute_PUSH_HL'Access;
      Table (16#F1#) := Definitions.Execute_POP_AF'Access;
      Table (16#F5#) := Definitions.Execute_PUSH_AF'Access;
      Table (16#18#) := Definitions.Execute_JR'Access;
      Table (16#20#) := Definitions.Execute_JR_NZ'Access;
      Table (16#28#) := Definitions.Execute_JR_Z'Access;
      Table (16#30#) := Definitions.Execute_JR_NC'Access;
      Table (16#38#) := Definitions.Execute_JR_C'Access;
      Table (16#C2#) := Definitions.Execute_JP_NZ'Access;
      Table (16#C3#) := Definitions.Execute_JP'Access;
      Table (16#CA#) := Definitions.Execute_JP_Z'Access;
      Table (16#D2#) := Definitions.Execute_JP_NC'Access;
      Table (16#DA#) := Definitions.Execute_JP_C'Access;
      Table (16#E9#) := Definitions.Execute_JP_HL'Access;
      Table (16#C4#) := Definitions.Execute_CALL_NZ'Access;
      Table (16#CD#) := Definitions.Execute_CALL'Access;
      Table (16#CC#) := Definitions.Execute_CALL_Z'Access;
      Table (16#D4#) := Definitions.Execute_CALL_NC'Access;
      Table (16#DC#) := Definitions.Execute_CALL_C'Access;
      Table (16#C0#) := Definitions.Execute_RET_NZ'Access;
      Table (16#C8#) := Definitions.Execute_RET_Z'Access;
      Table (16#C9#) := Definitions.Execute_RET'Access;
      Table (16#D0#) := Definitions.Execute_RET_NC'Access;
      Table (16#D8#) := Definitions.Execute_RET_C'Access;
      Table (16#D9#) := Definitions.Execute_RETI'Access;
      Table (16#C7#) := Definitions.Execute_RST_00'Access;
      Table (16#CF#) := Definitions.Execute_RST_08'Access;
      Table (16#D7#) := Definitions.Execute_RST_10'Access;
      Table (16#DF#) := Definitions.Execute_RST_18'Access;
      Table (16#E7#) := Definitions.Execute_RST_20'Access;
      Table (16#EF#) := Definitions.Execute_RST_28'Access;
      Table (16#F7#) := Definitions.Execute_RST_30'Access;
      Table (16#FF#) := Definitions.Execute_RST_38'Access;
      Table (16#E0#) := Definitions.Execute_LD_High_Addr_Imm8_A'Access;
      Table (16#E2#) := Definitions.Execute_LD_High_Addr_C_A'Access;
      Table (16#EA#) := Definitions.Execute_LD_Addr_Imm16_A'Access;
      Table (16#F0#) := Definitions.Execute_LD_A_High_Addr_Imm8'Access;
      Table (16#F2#) := Definitions.Execute_LD_A_High_Addr_C'Access;
      Table (16#F8#) := Definitions.Execute_LD_HL_SP_Plus_Imm8'Access;
      Table (16#F9#) := Definitions.Execute_LD_SP_HL'Access;
      Table (16#FA#) := Definitions.Execute_LD_A_Addr_Imm16'Access;
      Table (16#00#) := Definitions.Execute_NOP'Access;
      Table (16#09#) := Definitions.Execute_ADD_HL_BC'Access;
      Table (16#10#) := Definitions.Execute_STOP'Access;
      Table (16#19#) := Definitions.Execute_ADD_HL_DE'Access;
      Table (16#27#) := Definitions.Execute_DAA'Access;
      Table (16#29#) := Definitions.Execute_ADD_HL_HL'Access;
      Table (16#2F#) := Definitions.Execute_CPL'Access;
      Table (16#37#) := Definitions.Execute_SCF'Access;
      Table (16#39#) := Definitions.Execute_ADD_HL_SP'Access;
      Table (16#3F#) := Definitions.Execute_CCF'Access;
      Table (16#76#) := Definitions.Execute_HALT'Access;
      Table (16#81#) := Definitions.Execute_ADD_A_C'Access;
      Table (16#82#) := Definitions.Execute_ADD_A_D'Access;
      Table (16#83#) := Definitions.Execute_ADD_A_E'Access;
      Table (16#84#) := Definitions.Execute_ADD_A_H'Access;
      Table (16#85#) := Definitions.Execute_ADD_A_L'Access;
      Table (16#87#) := Definitions.Execute_ADD_A_A'Access;
      Table (16#88#) := Definitions.Execute_ADC_A_B'Access;
      Table (16#8A#) := Definitions.Execute_ADC_A_D'Access;
      Table (16#8B#) := Definitions.Execute_ADC_A_E'Access;
      Table (16#8C#) := Definitions.Execute_ADC_A_H'Access;
      Table (16#8D#) := Definitions.Execute_ADC_A_L'Access;
      Table (16#8E#) := Definitions.Execute_ADC_A_Addr_HL'Access;
      Table (16#8F#) := Definitions.Execute_ADC_A_A'Access;
      Table (16#90#) := Definitions.Execute_SUB_A_B'Access;
      Table (16#91#) := Definitions.Execute_SUB_A_C'Access;
      Table (16#93#) := Definitions.Execute_SUB_A_E'Access;
      Table (16#94#) := Definitions.Execute_SUB_A_H'Access;
      Table (16#95#) := Definitions.Execute_SUB_A_L'Access;
      Table (16#96#) := Definitions.Execute_SUB_A_Addr_HL'Access;
      Table (16#97#) := Definitions.Execute_SUB_A_A'Access;
      Table (16#98#) := Definitions.Execute_SBC_A_B'Access;
      Table (16#99#) := Definitions.Execute_SBC_A_C'Access;
      Table (16#9A#) := Definitions.Execute_SBC_A_D'Access;
      Table (16#9C#) := Definitions.Execute_SBC_A_H'Access;
      Table (16#9D#) := Definitions.Execute_SBC_A_L'Access;
      Table (16#9E#) := Definitions.Execute_SBC_A_Addr_HL'Access;
      Table (16#9F#) := Definitions.Execute_SBC_A_A'Access;
      Table (16#A0#) := Definitions.Execute_AND_A_B'Access;
      Table (16#A1#) := Definitions.Execute_AND_A_C'Access;
      Table (16#A2#) := Definitions.Execute_AND_A_D'Access;
      Table (16#A3#) := Definitions.Execute_AND_A_E'Access;
      Table (16#A5#) := Definitions.Execute_AND_A_L'Access;
      Table (16#A6#) := Definitions.Execute_AND_A_Addr_HL'Access;
      Table (16#A7#) := Definitions.Execute_AND_A_A'Access;
      Table (16#A8#) := Definitions.Execute_XOR_A_B'Access;
      Table (16#A9#) := Definitions.Execute_XOR_A_C'Access;
      Table (16#AA#) := Definitions.Execute_XOR_A_D'Access;
      Table (16#AB#) := Definitions.Execute_XOR_A_E'Access;
      Table (16#AC#) := Definitions.Execute_XOR_A_H'Access;
      Table (16#AE#) := Definitions.Execute_XOR_A_Addr_HL'Access;
      Table (16#AF#) := Definitions.Execute_XOR_A_A'Access;
      Table (16#B0#) := Definitions.Execute_OR_A_B'Access;
      Table (16#B1#) := Definitions.Execute_OR_A_C'Access;
      Table (16#B2#) := Definitions.Execute_OR_A_D'Access;
      Table (16#B3#) := Definitions.Execute_OR_A_E'Access;
      Table (16#B4#) := Definitions.Execute_OR_A_H'Access;
      Table (16#B5#) := Definitions.Execute_OR_A_L'Access;
      Table (16#B6#) := Definitions.Execute_OR_A_Addr_HL'Access;
      Table (16#B8#) := Definitions.Execute_CP_A_B'Access;
      Table (16#B9#) := Definitions.Execute_CP_A_C'Access;
      Table (16#BA#) := Definitions.Execute_CP_A_D'Access;
      Table (16#BB#) := Definitions.Execute_CP_A_E'Access;
      Table (16#BC#) := Definitions.Execute_CP_A_H'Access;
      Table (16#BD#) := Definitions.Execute_CP_A_L'Access;
      Table (16#BF#) := Definitions.Execute_CP_A_A'Access;
      Table (16#C6#) := Definitions.Execute_ADD_A_Imm8'Access;
      Table (16#CE#) := Definitions.Execute_ADC_A_Imm8'Access;
      Table (16#D6#) := Definitions.Execute_SUB_A_Imm8'Access;
      Table (16#DE#) := Definitions.Execute_SBC_A_Imm8'Access;
      Table (16#E6#) := Definitions.Execute_AND_A_Imm8'Access;
      Table (16#E8#) := Definitions.Execute_ADD_SP_Imm8'Access;
      Table (16#EE#) := Definitions.Execute_XOR_A_Imm8'Access;
      Table (16#F3#) := Definitions.Execute_DI'Access;
      Table (16#FB#) := Definitions.Execute_EI'Access;
      Table (16#FE#) := Definitions.Execute_CP_A_Imm8'Access;
      return Table;
   end Build_Main_Table;

   function Build_CB_Table return Handler_Table is
      Table : Handler_Table := [others => null];
   begin
      Table (16#40#) := Definitions.Execute_BIT_0_B'Access;
      Table (16#5E#) := Definitions.Execute_BIT_3_Addr_HL'Access;
      Table (16#A9#) := Definitions.Execute_RES_5_C'Access;
      Table (16#BE#) := Definitions.Execute_RES_7_Addr_HL'Access;
      Table (16#D7#) := Definitions.Execute_SET_2_A'Access;
      Table (16#F6#) := Definitions.Execute_SET_6_Addr_HL'Access;
      Table (16#00#) := Definitions.Execute_RLC_B'Access;
      Table (16#01#) := Definitions.Execute_RLC_C'Access;
      Table (16#02#) := Definitions.Execute_RLC_D'Access;
      Table (16#03#) := Definitions.Execute_RLC_E'Access;
      Table (16#04#) := Definitions.Execute_RLC_H'Access;
      Table (16#05#) := Definitions.Execute_RLC_L'Access;
      Table (16#06#) := Definitions.Execute_RLC_Addr_HL'Access;
      Table (16#07#) := Definitions.Execute_RLC_A'Access;
      Table (16#08#) := Definitions.Execute_RRC_B'Access;
      Table (16#09#) := Definitions.Execute_RRC_C'Access;
      Table (16#0A#) := Definitions.Execute_RRC_D'Access;
      Table (16#0B#) := Definitions.Execute_RRC_E'Access;
      Table (16#0C#) := Definitions.Execute_RRC_H'Access;
      Table (16#0D#) := Definitions.Execute_RRC_L'Access;
      Table (16#0E#) := Definitions.Execute_RRC_Addr_HL'Access;
      Table (16#0F#) := Definitions.Execute_RRC_A'Access;
      Table (16#10#) := Definitions.Execute_RL_B'Access;
      Table (16#11#) := Definitions.Execute_RL_C'Access;
      Table (16#12#) := Definitions.Execute_RL_D'Access;
      Table (16#13#) := Definitions.Execute_RL_E'Access;
      Table (16#14#) := Definitions.Execute_RL_H'Access;
      Table (16#15#) := Definitions.Execute_RL_L'Access;
      Table (16#16#) := Definitions.Execute_RL_Addr_HL'Access;
      Table (16#17#) := Definitions.Execute_RL_A'Access;
      Table (16#18#) := Definitions.Execute_RR_B'Access;
      Table (16#19#) := Definitions.Execute_RR_C'Access;
      Table (16#1A#) := Definitions.Execute_RR_D'Access;
      Table (16#1B#) := Definitions.Execute_RR_E'Access;
      Table (16#1C#) := Definitions.Execute_RR_H'Access;
      Table (16#1D#) := Definitions.Execute_RR_L'Access;
      Table (16#1E#) := Definitions.Execute_RR_Addr_HL'Access;
      Table (16#1F#) := Definitions.Execute_RR_A'Access;
      Table (16#20#) := Definitions.Execute_SLA_B'Access;
      Table (16#21#) := Definitions.Execute_SLA_C'Access;
      Table (16#22#) := Definitions.Execute_SLA_D'Access;
      Table (16#23#) := Definitions.Execute_SLA_E'Access;
      Table (16#24#) := Definitions.Execute_SLA_H'Access;
      Table (16#25#) := Definitions.Execute_SLA_L'Access;
      Table (16#26#) := Definitions.Execute_SLA_Addr_HL'Access;
      Table (16#27#) := Definitions.Execute_SLA_A'Access;
      Table (16#28#) := Definitions.Execute_SRA_B'Access;
      Table (16#29#) := Definitions.Execute_SRA_C'Access;
      Table (16#2A#) := Definitions.Execute_SRA_D'Access;
      Table (16#2B#) := Definitions.Execute_SRA_E'Access;
      Table (16#2C#) := Definitions.Execute_SRA_H'Access;
      Table (16#2D#) := Definitions.Execute_SRA_L'Access;
      Table (16#2E#) := Definitions.Execute_SRA_Addr_HL'Access;
      Table (16#2F#) := Definitions.Execute_SRA_A'Access;
      Table (16#30#) := Definitions.Execute_SWAP_B'Access;
      Table (16#31#) := Definitions.Execute_SWAP_C'Access;
      Table (16#32#) := Definitions.Execute_SWAP_D'Access;
      Table (16#33#) := Definitions.Execute_SWAP_E'Access;
      Table (16#34#) := Definitions.Execute_SWAP_H'Access;
      Table (16#35#) := Definitions.Execute_SWAP_L'Access;
      Table (16#36#) := Definitions.Execute_SWAP_Addr_HL'Access;
      Table (16#37#) := Definitions.Execute_SWAP_A'Access;
      Table (16#38#) := Definitions.Execute_SRL_B'Access;
      Table (16#39#) := Definitions.Execute_SRL_C'Access;
      Table (16#3A#) := Definitions.Execute_SRL_D'Access;
      Table (16#3B#) := Definitions.Execute_SRL_E'Access;
      Table (16#3C#) := Definitions.Execute_SRL_H'Access;
      Table (16#3D#) := Definitions.Execute_SRL_L'Access;
      Table (16#3E#) := Definitions.Execute_SRL_Addr_HL'Access;
      Table (16#3F#) := Definitions.Execute_SRL_A'Access;
      return Table;
   end Build_CB_Table;

   Main_Table : constant Handler_Table := Build_Main_Table;
   CB_Table   : constant Handler_Table := Build_CB_Table;

   function Main_Handler
     (Opcode : Byte) return Instruction_Handler is
   begin
      return Main_Table (Opcode);
   end Main_Handler;

   function CB_Handler
     (Opcode : Byte) return Instruction_Handler is
   begin
      return CB_Table (Opcode);
   end CB_Handler;

   function Read_Byte_Operand
     (GB      : in out Gade.GB.GB_Type;
      Operand :        Operand_Kind;
      Inst    :        Decoded_Instruction) return Byte is
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
         when OD_Addr_HL =>
            return Gade.GB.Memory_Map.Read_Byte (GB, GB.CPU.Regs.HL);
         when OD_Imm8 =>
            return Inst.Imm8;
         when others =>
            raise Program_Error;
      end case;
   end Read_Byte_Operand;

   procedure Write_Byte_Operand
     (GB      : in out Gade.GB.GB_Type;
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
         when OD_Addr_HL =>
            Gade.GB.Memory_Map.Write_Byte (GB, GB.CPU.Regs.HL, Value);
         when others =>
            raise Program_Error;
      end case;
   end Write_Byte_Operand;

   procedure Execute
     (GB          : in out Gade.GB.GB_Type;
      Instruction :        Decoded_Instruction) is
      Handler : Instruction_Handler;
      Value   : Byte;
      Word_Value : Word;
      Dummy   : Byte;
   begin
      case Instruction.Prefix is
         when Main =>
            Handler := Main_Handler (Instruction.Opcode);
            if Handler /= null then
               GB.CPU.PC := GB.CPU.PC + 1;
               Handler.all (GB);
               return;
            end if;

            GB.CPU.PC := GB.CPU.PC + Word (Instruction.Length);

            case Instruction.Operation is
               when OP_Invalid =>
                  raise Program_Error;
               when OP_NOP =>
                  null;
               when OP_ADD =>
                  if Instruction.Dest = OD_HL then
                     case Instruction.Src is
                        when OD_BC =>
                           Word_Value := GB.CPU.Regs.HL;
                           Do_Add (GB.CPU, GB.CPU.Regs.BC, Word_Value);
                           GB.CPU.Regs.HL := Word_Value;
                        when OD_DE =>
                           Word_Value := GB.CPU.Regs.HL;
                           Do_Add (GB.CPU, GB.CPU.Regs.DE, Word_Value);
                           GB.CPU.Regs.HL := Word_Value;
                        when OD_HL =>
                           Word_Value := GB.CPU.Regs.HL;
                           Do_Add (GB.CPU, GB.CPU.Regs.HL, Word_Value);
                           GB.CPU.Regs.HL := Word_Value;
                        when OD_SP =>
                           Word_Value := GB.CPU.Regs.HL;
                           Do_Add (GB.CPU, GB.CPU.Regs.SP, Word_Value);
                           GB.CPU.Regs.HL := Word_Value;
                        when others =>
                           raise Program_Error;
                     end case;
                  elsif Instruction.Dest = OD_SP and then Instruction.Src = OD_Rel8 then
                     Do_Add (GB.CPU, GB.CPU.Regs.SP, Instruction.Imm8);
                  else
                     Value := Read_Byte_Operand (GB, Instruction.Src, Instruction);
                     Do_Add (GB.CPU, Value, GB.CPU.Regs.A, ADD_Carry);
                  end if;
               when OP_ADC =>
                  Value := Read_Byte_Operand (GB, Instruction.Src, Instruction);
                  Do_Add (GB.CPU, Value, GB.CPU.Regs.A, ADC_Carry);
               when OP_SUB =>
                  Value := Read_Byte_Operand (GB, Instruction.Src, Instruction);
                  Do_Sub (GB.CPU, Value, GB.CPU.Regs.A, SUB_Carry);
               when OP_SBC =>
                  Value := Read_Byte_Operand (GB, Instruction.Src, Instruction);
                  Do_Sub (GB.CPU, Value, GB.CPU.Regs.A, SBC_Carry);
               when OP_AND =>
                  Do_AND (GB.CPU, Read_Byte_Operand (GB, Instruction.Src, Instruction));
               when OP_XOR =>
                  Do_XOR (GB.CPU, Read_Byte_Operand (GB, Instruction.Src, Instruction));
               when OP_OR =>
                  Do_OR (GB.CPU, Read_Byte_Operand (GB, Instruction.Src, Instruction));
               when OP_CP =>
                  Do_Sub
                    (GB.CPU,
                     Read_Byte_Operand (GB, Instruction.Src, Instruction),
                     Dummy,
                     SUB_Carry);
               when OP_DAA =>
                  Definitions.Execute_DAA (GB);
               when OP_CPL =>
                  Definitions.Execute_CPL (GB);
               when OP_SCF =>
                  Definitions.Execute_SCF (GB);
               when OP_CCF =>
                  Definitions.Execute_CCF (GB);
               when OP_HALT =>
                  Definitions.Execute_HALT (GB);
               when OP_STOP =>
                  Definitions.Execute_STOP (GB);
               when OP_DI =>
                  Definitions.Execute_DI (GB);
               when OP_EI =>
                  Definitions.Execute_EI (GB);
               when others =>
                  raise Program_Error;
            end case;

         when CB =>
            Handler := CB_Handler (Instruction.Opcode);
            if Handler /= null then
               GB.CPU.PC := GB.CPU.PC + 2;
               Handler.all (GB);
               return;
            end if;

            GB.CPU.PC := GB.CPU.PC + 2;

            case Instruction.Operation is
               when OP_Invalid =>
                  raise Program_Error;
               when OP_BIT =>
                  Do_Bit
                    (GB.CPU,
                     Bit_Index (Instruction.Bit_Index),
                     Read_Byte_Operand (GB, Instruction.Src, Instruction));
               when OP_SET =>
                  Value := Read_Byte_Operand (GB, Instruction.Dest, Instruction);
                  Do_Set_Bit
                    (GB.CPU,
                     SR_SET,
                     Bit_Index (Instruction.Bit_Index),
                     Value,
                     Value);
                  Write_Byte_Operand (GB, Instruction.Dest, Value);
               when OP_RES =>
                  Value := Read_Byte_Operand (GB, Instruction.Dest, Instruction);
                  Do_Set_Bit
                    (GB.CPU,
                     SR_RES,
                     Bit_Index (Instruction.Bit_Index),
                     Value,
                     Value);
                  Write_Byte_Operand (GB, Instruction.Dest, Value);
               when others =>
                  raise Program_Error;
            end case;
      end case;
   end Execute;

end Gade.Dev.CPU.Generic_Dispatch_Prototype;
