with Gade.Dev.CPU.Cycle_Steps;
with Gade.Dev.CPU.Instructions.Arithmetic;
with Gade.Dev.CPU.Instructions.Loads;
with Gade.Dev.CPU.Instructions.Bitwise;
with Gade.Dev.CPU.Instructions.Logic;
with Gade.Dev.CPU.Instructions.Stack;
with Gade.Dev.CPU.Instructions.Flow;
with Gade.Dev.CPU.Instructions.Control;
with Gade.GB.Memory_Map;

package body Gade.Dev.CPU.Dispatch is
   type Opcode_Prefix is (Main, CB);

      package Arithmetic renames Gade.Dev.CPU.Instructions.Arithmetic;
   package Loads renames Gade.Dev.CPU.Instructions.Loads;
   package Bitwise renames Gade.Dev.CPU.Instructions.Bitwise;
   package Logic renames Gade.Dev.CPU.Instructions.Logic;
   package Stack renames Gade.Dev.CPU.Instructions.Stack;
   package Flow renames Gade.Dev.CPU.Instructions.Flow;
   package Control renames Gade.Dev.CPU.Instructions.Control;
   type Handler_Table is array (Byte) of Instruction_Handler;

   function Build_Main_Table return Handler_Table;

   function Build_CB_Table return Handler_Table;

   function Hex_Digit (Value : Natural) return Character;

   function Hex_Byte (Value : Byte) return String;

   function Hex_Word (Value : Word) return String;

   function Prefix_Image (Prefix : Opcode_Prefix) return String;


   function Build_Main_Table return Handler_Table is
      Table : Handler_Table := [others => null];
   begin
      Table (16#80#) := Arithmetic.Execute_ADD_A_B'Access;
      Table (16#86#) := Arithmetic.Execute_ADD_A_Addr_HL'Access;
      Table (16#89#) := Arithmetic.Execute_ADC_A_C'Access;
      Table (16#92#) := Arithmetic.Execute_SUB_A_D'Access;
      Table (16#9B#) := Arithmetic.Execute_SBC_A_E'Access;
      Table (16#A4#) := Logic.Execute_AND_A_H'Access;
      Table (16#AD#) := Logic.Execute_XOR_A_L'Access;
      Table (16#B7#) := Logic.Execute_OR_A_A'Access;
      Table (16#BE#) := Logic.Execute_CP_A_Addr_HL'Access;
      Table (16#01#) := Loads.Execute_LD_BC_Imm16'Access;
      Table (16#02#) := Loads.Execute_LD_Addr_BC_A'Access;
      Table (16#06#) := Loads.Execute_LD_B_Imm8'Access;
      Table (16#08#) := Loads.Execute_LD_Addr_Imm16_SP'Access;
      Table (16#0A#) := Loads.Execute_LD_A_Addr_BC'Access;
      Table (16#0E#) := Loads.Execute_LD_C_Imm8'Access;
      Table (16#11#) := Loads.Execute_LD_DE_Imm16'Access;
      Table (16#12#) := Loads.Execute_LD_Addr_DE_A'Access;
      Table (16#16#) := Loads.Execute_LD_D_Imm8'Access;
      Table (16#1A#) := Loads.Execute_LD_A_Addr_DE'Access;
      Table (16#1E#) := Loads.Execute_LD_E_Imm8'Access;
      Table (16#21#) := Loads.Execute_LD_HL_Imm16'Access;
      Table (16#22#) := Loads.Execute_LD_Addr_HL_Inc_A'Access;
      Table (16#26#) := Loads.Execute_LD_H_Imm8'Access;
      Table (16#2A#) := Loads.Execute_LD_A_Addr_HL_Inc'Access;
      Table (16#2E#) := Loads.Execute_LD_L_Imm8'Access;
      Table (16#31#) := Loads.Execute_LD_SP_Imm16'Access;
      Table (16#32#) := Loads.Execute_LD_Addr_HL_Dec_A'Access;
      Table (16#36#) := Loads.Execute_LD_Addr_HL_Imm8'Access;
      Table (16#3A#) := Loads.Execute_LD_A_Addr_HL_Dec'Access;
      Table (16#3E#) := Loads.Execute_LD_A_Imm8'Access;
      Table (16#40#) := Loads.Execute_LD_B_B'Access;
      Table (16#41#) := Loads.Execute_LD_B_C'Access;
      Table (16#42#) := Loads.Execute_LD_B_D'Access;
      Table (16#43#) := Loads.Execute_LD_B_E'Access;
      Table (16#44#) := Loads.Execute_LD_B_H'Access;
      Table (16#45#) := Loads.Execute_LD_B_L'Access;
      Table (16#46#) := Loads.Execute_LD_B_Addr_HL'Access;
      Table (16#47#) := Loads.Execute_LD_B_A'Access;
      Table (16#48#) := Loads.Execute_LD_C_B'Access;
      Table (16#49#) := Loads.Execute_LD_C_C'Access;
      Table (16#4A#) := Loads.Execute_LD_C_D'Access;
      Table (16#4B#) := Loads.Execute_LD_C_E'Access;
      Table (16#4C#) := Loads.Execute_LD_C_H'Access;
      Table (16#4D#) := Loads.Execute_LD_C_L'Access;
      Table (16#4E#) := Loads.Execute_LD_C_Addr_HL'Access;
      Table (16#4F#) := Loads.Execute_LD_C_A'Access;
      Table (16#50#) := Loads.Execute_LD_D_B'Access;
      Table (16#51#) := Loads.Execute_LD_D_C'Access;
      Table (16#52#) := Loads.Execute_LD_D_D'Access;
      Table (16#53#) := Loads.Execute_LD_D_E'Access;
      Table (16#54#) := Loads.Execute_LD_D_H'Access;
      Table (16#55#) := Loads.Execute_LD_D_L'Access;
      Table (16#56#) := Loads.Execute_LD_D_Addr_HL'Access;
      Table (16#57#) := Loads.Execute_LD_D_A'Access;
      Table (16#58#) := Loads.Execute_LD_E_B'Access;
      Table (16#59#) := Loads.Execute_LD_E_C'Access;
      Table (16#5A#) := Loads.Execute_LD_E_D'Access;
      Table (16#5B#) := Loads.Execute_LD_E_E'Access;
      Table (16#5C#) := Loads.Execute_LD_E_H'Access;
      Table (16#5D#) := Loads.Execute_LD_E_L'Access;
      Table (16#5E#) := Loads.Execute_LD_E_Addr_HL'Access;
      Table (16#5F#) := Loads.Execute_LD_E_A'Access;
      Table (16#60#) := Loads.Execute_LD_H_B'Access;
      Table (16#61#) := Loads.Execute_LD_H_C'Access;
      Table (16#62#) := Loads.Execute_LD_H_D'Access;
      Table (16#63#) := Loads.Execute_LD_H_E'Access;
      Table (16#64#) := Loads.Execute_LD_H_H'Access;
      Table (16#65#) := Loads.Execute_LD_H_L'Access;
      Table (16#66#) := Loads.Execute_LD_H_Addr_HL'Access;
      Table (16#67#) := Loads.Execute_LD_H_A'Access;
      Table (16#68#) := Loads.Execute_LD_L_B'Access;
      Table (16#69#) := Loads.Execute_LD_L_C'Access;
      Table (16#6A#) := Loads.Execute_LD_L_D'Access;
      Table (16#6B#) := Loads.Execute_LD_L_E'Access;
      Table (16#6C#) := Loads.Execute_LD_L_H'Access;
      Table (16#6D#) := Loads.Execute_LD_L_L'Access;
      Table (16#6E#) := Loads.Execute_LD_L_Addr_HL'Access;
      Table (16#6F#) := Loads.Execute_LD_L_A'Access;
      Table (16#70#) := Loads.Execute_LD_Addr_HL_B'Access;
      Table (16#71#) := Loads.Execute_LD_Addr_HL_C'Access;
      Table (16#72#) := Loads.Execute_LD_Addr_HL_D'Access;
      Table (16#73#) := Loads.Execute_LD_Addr_HL_E'Access;
      Table (16#74#) := Loads.Execute_LD_Addr_HL_H'Access;
      Table (16#75#) := Loads.Execute_LD_Addr_HL_L'Access;
      Table (16#77#) := Loads.Execute_LD_Addr_HL_A'Access;
      Table (16#78#) := Loads.Execute_LD_A_B'Access;
      Table (16#79#) := Loads.Execute_LD_A_C'Access;
      Table (16#7A#) := Loads.Execute_LD_A_D'Access;
      Table (16#7B#) := Loads.Execute_LD_A_E'Access;
      Table (16#7C#) := Loads.Execute_LD_A_H'Access;
      Table (16#7D#) := Loads.Execute_LD_A_L'Access;
      Table (16#7E#) := Loads.Execute_LD_A_Addr_HL'Access;
      Table (16#7F#) := Loads.Execute_LD_A_A'Access;
      Table (16#3E#) := Loads.Execute_LD_A_Imm8'Access;
      Table (16#36#) := Loads.Execute_LD_Addr_HL_Imm8'Access;
      Table (16#03#) := Arithmetic.Execute_INC_BC'Access;
      Table (16#04#) := Arithmetic.Execute_INC_B'Access;
      Table (16#05#) := Arithmetic.Execute_DEC_B'Access;
      Table (16#0B#) := Arithmetic.Execute_DEC_BC'Access;
      Table (16#0C#) := Arithmetic.Execute_INC_C'Access;
      Table (16#0D#) := Arithmetic.Execute_DEC_C'Access;
      Table (16#13#) := Arithmetic.Execute_INC_DE'Access;
      Table (16#14#) := Arithmetic.Execute_INC_D'Access;
      Table (16#15#) := Arithmetic.Execute_DEC_D'Access;
      Table (16#1B#) := Arithmetic.Execute_DEC_DE'Access;
      Table (16#1C#) := Arithmetic.Execute_INC_E'Access;
      Table (16#1D#) := Arithmetic.Execute_DEC_E'Access;
      Table (16#35#) := Arithmetic.Execute_DEC_Addr_HL'Access;
      Table (16#23#) := Arithmetic.Execute_INC_HL'Access;
      Table (16#24#) := Arithmetic.Execute_INC_H'Access;
      Table (16#25#) := Arithmetic.Execute_DEC_H'Access;
      Table (16#2B#) := Arithmetic.Execute_DEC_HL'Access;
      Table (16#2C#) := Arithmetic.Execute_INC_L'Access;
      Table (16#2D#) := Arithmetic.Execute_DEC_L'Access;
      Table (16#33#) := Arithmetic.Execute_INC_SP'Access;
      Table (16#34#) := Arithmetic.Execute_INC_Addr_HL'Access;
      Table (16#3B#) := Arithmetic.Execute_DEC_SP'Access;
      Table (16#3C#) := Arithmetic.Execute_INC_A'Access;
      Table (16#3D#) := Arithmetic.Execute_DEC_A'Access;
      Table (16#07#) := Bitwise.Execute_RLCA'Access;
      Table (16#0F#) := Bitwise.Execute_RRCA'Access;
      Table (16#17#) := Bitwise.Execute_RLA'Access;
      Table (16#1F#) := Bitwise.Execute_RRA'Access;
      Table (16#C1#) := Stack.Execute_POP_BC'Access;
      Table (16#C5#) := Stack.Execute_PUSH_BC'Access;
      Table (16#D1#) := Stack.Execute_POP_DE'Access;
      Table (16#D5#) := Stack.Execute_PUSH_DE'Access;
      Table (16#E1#) := Stack.Execute_POP_HL'Access;
      Table (16#E5#) := Stack.Execute_PUSH_HL'Access;
      Table (16#F1#) := Stack.Execute_POP_AF'Access;
      Table (16#F5#) := Stack.Execute_PUSH_AF'Access;
      Table (16#18#) := Flow.Execute_JR'Access;
      Table (16#20#) := Flow.Execute_JR_NZ'Access;
      Table (16#28#) := Flow.Execute_JR_Z'Access;
      Table (16#30#) := Flow.Execute_JR_NC'Access;
      Table (16#38#) := Flow.Execute_JR_C'Access;
      Table (16#C2#) := Flow.Execute_JP_NZ'Access;
      Table (16#C3#) := Flow.Execute_JP'Access;
      Table (16#CA#) := Flow.Execute_JP_Z'Access;
      Table (16#D2#) := Flow.Execute_JP_NC'Access;
      Table (16#DA#) := Flow.Execute_JP_C'Access;
      Table (16#E9#) := Flow.Execute_JP_HL'Access;
      Table (16#C4#) := Flow.Execute_CALL_NZ'Access;
      Table (16#CD#) := Flow.Execute_CALL'Access;
      Table (16#CC#) := Flow.Execute_CALL_Z'Access;
      Table (16#D4#) := Flow.Execute_CALL_NC'Access;
      Table (16#DC#) := Flow.Execute_CALL_C'Access;
      Table (16#C0#) := Flow.Execute_RET_NZ'Access;
      Table (16#C8#) := Flow.Execute_RET_Z'Access;
      Table (16#C9#) := Flow.Execute_RET'Access;
      Table (16#D0#) := Flow.Execute_RET_NC'Access;
      Table (16#D8#) := Flow.Execute_RET_C'Access;
      Table (16#D9#) := Flow.Execute_RETI'Access;
      Table (16#C7#) := Flow.Execute_RST_00'Access;
      Table (16#CF#) := Flow.Execute_RST_08'Access;
      Table (16#D7#) := Flow.Execute_RST_10'Access;
      Table (16#DF#) := Flow.Execute_RST_18'Access;
      Table (16#E7#) := Flow.Execute_RST_20'Access;
      Table (16#EF#) := Flow.Execute_RST_28'Access;
      Table (16#F7#) := Flow.Execute_RST_30'Access;
      Table (16#FF#) := Flow.Execute_RST_38'Access;
      Table (16#E0#) := Loads.Execute_LD_High_Addr_Imm8_A'Access;
      Table (16#E2#) := Loads.Execute_LD_High_Addr_C_A'Access;
      Table (16#EA#) := Loads.Execute_LD_Addr_Imm16_A'Access;
      Table (16#F0#) := Loads.Execute_LD_A_High_Addr_Imm8'Access;
      Table (16#F2#) := Loads.Execute_LD_A_High_Addr_C'Access;
      Table (16#F8#) := Loads.Execute_LD_HL_SP_Plus_Imm8'Access;
      Table (16#F9#) := Loads.Execute_LD_SP_HL'Access;
      Table (16#FA#) := Loads.Execute_LD_A_Addr_Imm16'Access;
      Table (16#00#) := Control.Execute_NOP'Access;
      Table (16#09#) := Arithmetic.Execute_ADD_HL_BC'Access;
      Table (16#10#) := Control.Execute_STOP'Access;
      Table (16#19#) := Arithmetic.Execute_ADD_HL_DE'Access;
      Table (16#27#) := Arithmetic.Execute_DAA'Access;
      Table (16#29#) := Arithmetic.Execute_ADD_HL_HL'Access;
      Table (16#2F#) := Control.Execute_CPL'Access;
      Table (16#37#) := Control.Execute_SCF'Access;
      Table (16#39#) := Arithmetic.Execute_ADD_HL_SP'Access;
      Table (16#3F#) := Control.Execute_CCF'Access;
      Table (16#76#) := Control.Execute_HALT'Access;
      Table (16#81#) := Arithmetic.Execute_ADD_A_C'Access;
      Table (16#82#) := Arithmetic.Execute_ADD_A_D'Access;
      Table (16#83#) := Arithmetic.Execute_ADD_A_E'Access;
      Table (16#84#) := Arithmetic.Execute_ADD_A_H'Access;
      Table (16#85#) := Arithmetic.Execute_ADD_A_L'Access;
      Table (16#87#) := Arithmetic.Execute_ADD_A_A'Access;
      Table (16#88#) := Arithmetic.Execute_ADC_A_B'Access;
      Table (16#8A#) := Arithmetic.Execute_ADC_A_D'Access;
      Table (16#8B#) := Arithmetic.Execute_ADC_A_E'Access;
      Table (16#8C#) := Arithmetic.Execute_ADC_A_H'Access;
      Table (16#8D#) := Arithmetic.Execute_ADC_A_L'Access;
      Table (16#8E#) := Arithmetic.Execute_ADC_A_Addr_HL'Access;
      Table (16#8F#) := Arithmetic.Execute_ADC_A_A'Access;
      Table (16#90#) := Arithmetic.Execute_SUB_A_B'Access;
      Table (16#91#) := Arithmetic.Execute_SUB_A_C'Access;
      Table (16#93#) := Arithmetic.Execute_SUB_A_E'Access;
      Table (16#94#) := Arithmetic.Execute_SUB_A_H'Access;
      Table (16#95#) := Arithmetic.Execute_SUB_A_L'Access;
      Table (16#96#) := Arithmetic.Execute_SUB_A_Addr_HL'Access;
      Table (16#97#) := Arithmetic.Execute_SUB_A_A'Access;
      Table (16#98#) := Arithmetic.Execute_SBC_A_B'Access;
      Table (16#99#) := Arithmetic.Execute_SBC_A_C'Access;
      Table (16#9A#) := Arithmetic.Execute_SBC_A_D'Access;
      Table (16#9C#) := Arithmetic.Execute_SBC_A_H'Access;
      Table (16#9D#) := Arithmetic.Execute_SBC_A_L'Access;
      Table (16#9E#) := Arithmetic.Execute_SBC_A_Addr_HL'Access;
      Table (16#9F#) := Arithmetic.Execute_SBC_A_A'Access;
      Table (16#A0#) := Logic.Execute_AND_A_B'Access;
      Table (16#A1#) := Logic.Execute_AND_A_C'Access;
      Table (16#A2#) := Logic.Execute_AND_A_D'Access;
      Table (16#A3#) := Logic.Execute_AND_A_E'Access;
      Table (16#A5#) := Logic.Execute_AND_A_L'Access;
      Table (16#A6#) := Logic.Execute_AND_A_Addr_HL'Access;
      Table (16#A7#) := Logic.Execute_AND_A_A'Access;
      Table (16#A8#) := Logic.Execute_XOR_A_B'Access;
      Table (16#A9#) := Logic.Execute_XOR_A_C'Access;
      Table (16#AA#) := Logic.Execute_XOR_A_D'Access;
      Table (16#AB#) := Logic.Execute_XOR_A_E'Access;
      Table (16#AC#) := Logic.Execute_XOR_A_H'Access;
      Table (16#AE#) := Logic.Execute_XOR_A_Addr_HL'Access;
      Table (16#AF#) := Logic.Execute_XOR_A_A'Access;
      Table (16#B0#) := Logic.Execute_OR_A_B'Access;
      Table (16#B1#) := Logic.Execute_OR_A_C'Access;
      Table (16#B2#) := Logic.Execute_OR_A_D'Access;
      Table (16#B3#) := Logic.Execute_OR_A_E'Access;
      Table (16#B4#) := Logic.Execute_OR_A_H'Access;
      Table (16#B5#) := Logic.Execute_OR_A_L'Access;
      Table (16#B6#) := Logic.Execute_OR_A_Addr_HL'Access;
      Table (16#B8#) := Logic.Execute_CP_A_B'Access;
      Table (16#B9#) := Logic.Execute_CP_A_C'Access;
      Table (16#BA#) := Logic.Execute_CP_A_D'Access;
      Table (16#BB#) := Logic.Execute_CP_A_E'Access;
      Table (16#BC#) := Logic.Execute_CP_A_H'Access;
      Table (16#BD#) := Logic.Execute_CP_A_L'Access;
      Table (16#BF#) := Logic.Execute_CP_A_A'Access;
      Table (16#C6#) := Arithmetic.Execute_ADD_A_Imm8'Access;
      Table (16#CE#) := Arithmetic.Execute_ADC_A_Imm8'Access;
      Table (16#D6#) := Arithmetic.Execute_SUB_A_Imm8'Access;
      Table (16#DE#) := Arithmetic.Execute_SBC_A_Imm8'Access;
      Table (16#E6#) := Logic.Execute_AND_A_Imm8'Access;
      Table (16#E8#) := Arithmetic.Execute_ADD_SP_Imm8'Access;
      Table (16#EE#) := Logic.Execute_XOR_A_Imm8'Access;
      Table (16#F3#) := Control.Execute_DI'Access;
      Table (16#F6#) := Logic.Execute_OR_A_Imm8'Access;
      Table (16#FB#) := Control.Execute_EI'Access;
      Table (16#FE#) := Logic.Execute_CP_A_Imm8'Access;
      return Table;
   end Build_Main_Table;

   function Build_CB_Table return Handler_Table is
      Table : Handler_Table := [others => null];
   begin
      Table (16#00#) := Bitwise.Execute_RLC_B'Access;
      Table (16#01#) := Bitwise.Execute_RLC_C'Access;
      Table (16#02#) := Bitwise.Execute_RLC_D'Access;
      Table (16#03#) := Bitwise.Execute_RLC_E'Access;
      Table (16#04#) := Bitwise.Execute_RLC_H'Access;
      Table (16#05#) := Bitwise.Execute_RLC_L'Access;
      Table (16#06#) := Bitwise.Execute_RLC_Addr_HL'Access;
      Table (16#07#) := Bitwise.Execute_RLC_A'Access;
      Table (16#08#) := Bitwise.Execute_RRC_B'Access;
      Table (16#09#) := Bitwise.Execute_RRC_C'Access;
      Table (16#0A#) := Bitwise.Execute_RRC_D'Access;
      Table (16#0B#) := Bitwise.Execute_RRC_E'Access;
      Table (16#0C#) := Bitwise.Execute_RRC_H'Access;
      Table (16#0D#) := Bitwise.Execute_RRC_L'Access;
      Table (16#0E#) := Bitwise.Execute_RRC_Addr_HL'Access;
      Table (16#0F#) := Bitwise.Execute_RRC_A'Access;
      Table (16#10#) := Bitwise.Execute_RL_B'Access;
      Table (16#11#) := Bitwise.Execute_RL_C'Access;
      Table (16#12#) := Bitwise.Execute_RL_D'Access;
      Table (16#13#) := Bitwise.Execute_RL_E'Access;
      Table (16#14#) := Bitwise.Execute_RL_H'Access;
      Table (16#15#) := Bitwise.Execute_RL_L'Access;
      Table (16#16#) := Bitwise.Execute_RL_Addr_HL'Access;
      Table (16#17#) := Bitwise.Execute_RL_A'Access;
      Table (16#18#) := Bitwise.Execute_RR_B'Access;
      Table (16#19#) := Bitwise.Execute_RR_C'Access;
      Table (16#1A#) := Bitwise.Execute_RR_D'Access;
      Table (16#1B#) := Bitwise.Execute_RR_E'Access;
      Table (16#1C#) := Bitwise.Execute_RR_H'Access;
      Table (16#1D#) := Bitwise.Execute_RR_L'Access;
      Table (16#1E#) := Bitwise.Execute_RR_Addr_HL'Access;
      Table (16#1F#) := Bitwise.Execute_RR_A'Access;
      Table (16#20#) := Bitwise.Execute_SLA_B'Access;
      Table (16#21#) := Bitwise.Execute_SLA_C'Access;
      Table (16#22#) := Bitwise.Execute_SLA_D'Access;
      Table (16#23#) := Bitwise.Execute_SLA_E'Access;
      Table (16#24#) := Bitwise.Execute_SLA_H'Access;
      Table (16#25#) := Bitwise.Execute_SLA_L'Access;
      Table (16#26#) := Bitwise.Execute_SLA_Addr_HL'Access;
      Table (16#27#) := Bitwise.Execute_SLA_A'Access;
      Table (16#28#) := Bitwise.Execute_SRA_B'Access;
      Table (16#29#) := Bitwise.Execute_SRA_C'Access;
      Table (16#2A#) := Bitwise.Execute_SRA_D'Access;
      Table (16#2B#) := Bitwise.Execute_SRA_E'Access;
      Table (16#2C#) := Bitwise.Execute_SRA_H'Access;
      Table (16#2D#) := Bitwise.Execute_SRA_L'Access;
      Table (16#2E#) := Bitwise.Execute_SRA_Addr_HL'Access;
      Table (16#2F#) := Bitwise.Execute_SRA_A'Access;
      Table (16#30#) := Bitwise.Execute_SWAP_B'Access;
      Table (16#31#) := Bitwise.Execute_SWAP_C'Access;
      Table (16#32#) := Bitwise.Execute_SWAP_D'Access;
      Table (16#33#) := Bitwise.Execute_SWAP_E'Access;
      Table (16#34#) := Bitwise.Execute_SWAP_H'Access;
      Table (16#35#) := Bitwise.Execute_SWAP_L'Access;
      Table (16#36#) := Bitwise.Execute_SWAP_Addr_HL'Access;
      Table (16#37#) := Bitwise.Execute_SWAP_A'Access;
      Table (16#38#) := Bitwise.Execute_SRL_B'Access;
      Table (16#39#) := Bitwise.Execute_SRL_C'Access;
      Table (16#3A#) := Bitwise.Execute_SRL_D'Access;
      Table (16#3B#) := Bitwise.Execute_SRL_E'Access;
      Table (16#3C#) := Bitwise.Execute_SRL_H'Access;
      Table (16#3D#) := Bitwise.Execute_SRL_L'Access;
      Table (16#3E#) := Bitwise.Execute_SRL_Addr_HL'Access;
      Table (16#3F#) := Bitwise.Execute_SRL_A'Access;
      Table (16#40#) := Bitwise.Execute_BIT_0_B'Access;
      Table (16#41#) := Bitwise.Execute_BIT_0_C'Access;
      Table (16#42#) := Bitwise.Execute_BIT_0_D'Access;
      Table (16#43#) := Bitwise.Execute_BIT_0_E'Access;
      Table (16#44#) := Bitwise.Execute_BIT_0_H'Access;
      Table (16#45#) := Bitwise.Execute_BIT_0_L'Access;
      Table (16#46#) := Bitwise.Execute_BIT_0_Addr_HL'Access;
      Table (16#47#) := Bitwise.Execute_BIT_0_A'Access;
      Table (16#48#) := Bitwise.Execute_BIT_1_B'Access;
      Table (16#49#) := Bitwise.Execute_BIT_1_C'Access;
      Table (16#4A#) := Bitwise.Execute_BIT_1_D'Access;
      Table (16#4B#) := Bitwise.Execute_BIT_1_E'Access;
      Table (16#4C#) := Bitwise.Execute_BIT_1_H'Access;
      Table (16#4D#) := Bitwise.Execute_BIT_1_L'Access;
      Table (16#4E#) := Bitwise.Execute_BIT_1_Addr_HL'Access;
      Table (16#4F#) := Bitwise.Execute_BIT_1_A'Access;
      Table (16#50#) := Bitwise.Execute_BIT_2_B'Access;
      Table (16#51#) := Bitwise.Execute_BIT_2_C'Access;
      Table (16#52#) := Bitwise.Execute_BIT_2_D'Access;
      Table (16#53#) := Bitwise.Execute_BIT_2_E'Access;
      Table (16#54#) := Bitwise.Execute_BIT_2_H'Access;
      Table (16#55#) := Bitwise.Execute_BIT_2_L'Access;
      Table (16#56#) := Bitwise.Execute_BIT_2_Addr_HL'Access;
      Table (16#57#) := Bitwise.Execute_BIT_2_A'Access;
      Table (16#58#) := Bitwise.Execute_BIT_3_B'Access;
      Table (16#59#) := Bitwise.Execute_BIT_3_C'Access;
      Table (16#5A#) := Bitwise.Execute_BIT_3_D'Access;
      Table (16#5B#) := Bitwise.Execute_BIT_3_E'Access;
      Table (16#5C#) := Bitwise.Execute_BIT_3_H'Access;
      Table (16#5D#) := Bitwise.Execute_BIT_3_L'Access;
      Table (16#5E#) := Bitwise.Execute_BIT_3_Addr_HL'Access;
      Table (16#5F#) := Bitwise.Execute_BIT_3_A'Access;
      Table (16#60#) := Bitwise.Execute_BIT_4_B'Access;
      Table (16#61#) := Bitwise.Execute_BIT_4_C'Access;
      Table (16#62#) := Bitwise.Execute_BIT_4_D'Access;
      Table (16#63#) := Bitwise.Execute_BIT_4_E'Access;
      Table (16#64#) := Bitwise.Execute_BIT_4_H'Access;
      Table (16#65#) := Bitwise.Execute_BIT_4_L'Access;
      Table (16#66#) := Bitwise.Execute_BIT_4_Addr_HL'Access;
      Table (16#67#) := Bitwise.Execute_BIT_4_A'Access;
      Table (16#68#) := Bitwise.Execute_BIT_5_B'Access;
      Table (16#69#) := Bitwise.Execute_BIT_5_C'Access;
      Table (16#6A#) := Bitwise.Execute_BIT_5_D'Access;
      Table (16#6B#) := Bitwise.Execute_BIT_5_E'Access;
      Table (16#6C#) := Bitwise.Execute_BIT_5_H'Access;
      Table (16#6D#) := Bitwise.Execute_BIT_5_L'Access;
      Table (16#6E#) := Bitwise.Execute_BIT_5_Addr_HL'Access;
      Table (16#6F#) := Bitwise.Execute_BIT_5_A'Access;
      Table (16#70#) := Bitwise.Execute_BIT_6_B'Access;
      Table (16#71#) := Bitwise.Execute_BIT_6_C'Access;
      Table (16#72#) := Bitwise.Execute_BIT_6_D'Access;
      Table (16#73#) := Bitwise.Execute_BIT_6_E'Access;
      Table (16#74#) := Bitwise.Execute_BIT_6_H'Access;
      Table (16#75#) := Bitwise.Execute_BIT_6_L'Access;
      Table (16#76#) := Bitwise.Execute_BIT_6_Addr_HL'Access;
      Table (16#77#) := Bitwise.Execute_BIT_6_A'Access;
      Table (16#78#) := Bitwise.Execute_BIT_7_B'Access;
      Table (16#79#) := Bitwise.Execute_BIT_7_C'Access;
      Table (16#7A#) := Bitwise.Execute_BIT_7_D'Access;
      Table (16#7B#) := Bitwise.Execute_BIT_7_E'Access;
      Table (16#7C#) := Bitwise.Execute_BIT_7_H'Access;
      Table (16#7D#) := Bitwise.Execute_BIT_7_L'Access;
      Table (16#7E#) := Bitwise.Execute_BIT_7_Addr_HL'Access;
      Table (16#7F#) := Bitwise.Execute_BIT_7_A'Access;
      Table (16#80#) := Bitwise.Execute_RES_0_B'Access;
      Table (16#81#) := Bitwise.Execute_RES_0_C'Access;
      Table (16#82#) := Bitwise.Execute_RES_0_D'Access;
      Table (16#83#) := Bitwise.Execute_RES_0_E'Access;
      Table (16#84#) := Bitwise.Execute_RES_0_H'Access;
      Table (16#85#) := Bitwise.Execute_RES_0_L'Access;
      Table (16#86#) := Bitwise.Execute_RES_0_Addr_HL'Access;
      Table (16#87#) := Bitwise.Execute_RES_0_A'Access;
      Table (16#88#) := Bitwise.Execute_RES_1_B'Access;
      Table (16#89#) := Bitwise.Execute_RES_1_C'Access;
      Table (16#8A#) := Bitwise.Execute_RES_1_D'Access;
      Table (16#8B#) := Bitwise.Execute_RES_1_E'Access;
      Table (16#8C#) := Bitwise.Execute_RES_1_H'Access;
      Table (16#8D#) := Bitwise.Execute_RES_1_L'Access;
      Table (16#8E#) := Bitwise.Execute_RES_1_Addr_HL'Access;
      Table (16#8F#) := Bitwise.Execute_RES_1_A'Access;
      Table (16#90#) := Bitwise.Execute_RES_2_B'Access;
      Table (16#91#) := Bitwise.Execute_RES_2_C'Access;
      Table (16#92#) := Bitwise.Execute_RES_2_D'Access;
      Table (16#93#) := Bitwise.Execute_RES_2_E'Access;
      Table (16#94#) := Bitwise.Execute_RES_2_H'Access;
      Table (16#95#) := Bitwise.Execute_RES_2_L'Access;
      Table (16#96#) := Bitwise.Execute_RES_2_Addr_HL'Access;
      Table (16#97#) := Bitwise.Execute_RES_2_A'Access;
      Table (16#98#) := Bitwise.Execute_RES_3_B'Access;
      Table (16#99#) := Bitwise.Execute_RES_3_C'Access;
      Table (16#9A#) := Bitwise.Execute_RES_3_D'Access;
      Table (16#9B#) := Bitwise.Execute_RES_3_E'Access;
      Table (16#9C#) := Bitwise.Execute_RES_3_H'Access;
      Table (16#9D#) := Bitwise.Execute_RES_3_L'Access;
      Table (16#9E#) := Bitwise.Execute_RES_3_Addr_HL'Access;
      Table (16#9F#) := Bitwise.Execute_RES_3_A'Access;
      Table (16#A0#) := Bitwise.Execute_RES_4_B'Access;
      Table (16#A1#) := Bitwise.Execute_RES_4_C'Access;
      Table (16#A2#) := Bitwise.Execute_RES_4_D'Access;
      Table (16#A3#) := Bitwise.Execute_RES_4_E'Access;
      Table (16#A4#) := Bitwise.Execute_RES_4_H'Access;
      Table (16#A5#) := Bitwise.Execute_RES_4_L'Access;
      Table (16#A6#) := Bitwise.Execute_RES_4_Addr_HL'Access;
      Table (16#A7#) := Bitwise.Execute_RES_4_A'Access;
      Table (16#A8#) := Bitwise.Execute_RES_5_B'Access;
      Table (16#A9#) := Bitwise.Execute_RES_5_C'Access;
      Table (16#AA#) := Bitwise.Execute_RES_5_D'Access;
      Table (16#AB#) := Bitwise.Execute_RES_5_E'Access;
      Table (16#AC#) := Bitwise.Execute_RES_5_H'Access;
      Table (16#AD#) := Bitwise.Execute_RES_5_L'Access;
      Table (16#AE#) := Bitwise.Execute_RES_5_Addr_HL'Access;
      Table (16#AF#) := Bitwise.Execute_RES_5_A'Access;
      Table (16#B0#) := Bitwise.Execute_RES_6_B'Access;
      Table (16#B1#) := Bitwise.Execute_RES_6_C'Access;
      Table (16#B2#) := Bitwise.Execute_RES_6_D'Access;
      Table (16#B3#) := Bitwise.Execute_RES_6_E'Access;
      Table (16#B4#) := Bitwise.Execute_RES_6_H'Access;
      Table (16#B5#) := Bitwise.Execute_RES_6_L'Access;
      Table (16#B6#) := Bitwise.Execute_RES_6_Addr_HL'Access;
      Table (16#B7#) := Bitwise.Execute_RES_6_A'Access;
      Table (16#B8#) := Bitwise.Execute_RES_7_B'Access;
      Table (16#B9#) := Bitwise.Execute_RES_7_C'Access;
      Table (16#BA#) := Bitwise.Execute_RES_7_D'Access;
      Table (16#BB#) := Bitwise.Execute_RES_7_E'Access;
      Table (16#BC#) := Bitwise.Execute_RES_7_H'Access;
      Table (16#BD#) := Bitwise.Execute_RES_7_L'Access;
      Table (16#BE#) := Bitwise.Execute_RES_7_Addr_HL'Access;
      Table (16#BF#) := Bitwise.Execute_RES_7_A'Access;
      Table (16#C0#) := Bitwise.Execute_SET_0_B'Access;
      Table (16#C1#) := Bitwise.Execute_SET_0_C'Access;
      Table (16#C2#) := Bitwise.Execute_SET_0_D'Access;
      Table (16#C3#) := Bitwise.Execute_SET_0_E'Access;
      Table (16#C4#) := Bitwise.Execute_SET_0_H'Access;
      Table (16#C5#) := Bitwise.Execute_SET_0_L'Access;
      Table (16#C6#) := Bitwise.Execute_SET_0_Addr_HL'Access;
      Table (16#C7#) := Bitwise.Execute_SET_0_A'Access;
      Table (16#C8#) := Bitwise.Execute_SET_1_B'Access;
      Table (16#C9#) := Bitwise.Execute_SET_1_C'Access;
      Table (16#CA#) := Bitwise.Execute_SET_1_D'Access;
      Table (16#CB#) := Bitwise.Execute_SET_1_E'Access;
      Table (16#CC#) := Bitwise.Execute_SET_1_H'Access;
      Table (16#CD#) := Bitwise.Execute_SET_1_L'Access;
      Table (16#CE#) := Bitwise.Execute_SET_1_Addr_HL'Access;
      Table (16#CF#) := Bitwise.Execute_SET_1_A'Access;
      Table (16#D0#) := Bitwise.Execute_SET_2_B'Access;
      Table (16#D1#) := Bitwise.Execute_SET_2_C'Access;
      Table (16#D2#) := Bitwise.Execute_SET_2_D'Access;
      Table (16#D3#) := Bitwise.Execute_SET_2_E'Access;
      Table (16#D4#) := Bitwise.Execute_SET_2_H'Access;
      Table (16#D5#) := Bitwise.Execute_SET_2_L'Access;
      Table (16#D6#) := Bitwise.Execute_SET_2_Addr_HL'Access;
      Table (16#D7#) := Bitwise.Execute_SET_2_A'Access;
      Table (16#D8#) := Bitwise.Execute_SET_3_B'Access;
      Table (16#D9#) := Bitwise.Execute_SET_3_C'Access;
      Table (16#DA#) := Bitwise.Execute_SET_3_D'Access;
      Table (16#DB#) := Bitwise.Execute_SET_3_E'Access;
      Table (16#DC#) := Bitwise.Execute_SET_3_H'Access;
      Table (16#DD#) := Bitwise.Execute_SET_3_L'Access;
      Table (16#DE#) := Bitwise.Execute_SET_3_Addr_HL'Access;
      Table (16#DF#) := Bitwise.Execute_SET_3_A'Access;
      Table (16#E0#) := Bitwise.Execute_SET_4_B'Access;
      Table (16#E1#) := Bitwise.Execute_SET_4_C'Access;
      Table (16#E2#) := Bitwise.Execute_SET_4_D'Access;
      Table (16#E3#) := Bitwise.Execute_SET_4_E'Access;
      Table (16#E4#) := Bitwise.Execute_SET_4_H'Access;
      Table (16#E5#) := Bitwise.Execute_SET_4_L'Access;
      Table (16#E6#) := Bitwise.Execute_SET_4_Addr_HL'Access;
      Table (16#E7#) := Bitwise.Execute_SET_4_A'Access;
      Table (16#E8#) := Bitwise.Execute_SET_5_B'Access;
      Table (16#E9#) := Bitwise.Execute_SET_5_C'Access;
      Table (16#EA#) := Bitwise.Execute_SET_5_D'Access;
      Table (16#EB#) := Bitwise.Execute_SET_5_E'Access;
      Table (16#EC#) := Bitwise.Execute_SET_5_H'Access;
      Table (16#ED#) := Bitwise.Execute_SET_5_L'Access;
      Table (16#EE#) := Bitwise.Execute_SET_5_Addr_HL'Access;
      Table (16#EF#) := Bitwise.Execute_SET_5_A'Access;
      Table (16#F0#) := Bitwise.Execute_SET_6_B'Access;
      Table (16#F1#) := Bitwise.Execute_SET_6_C'Access;
      Table (16#F2#) := Bitwise.Execute_SET_6_D'Access;
      Table (16#F3#) := Bitwise.Execute_SET_6_E'Access;
      Table (16#F4#) := Bitwise.Execute_SET_6_H'Access;
      Table (16#F5#) := Bitwise.Execute_SET_6_L'Access;
      Table (16#F6#) := Bitwise.Execute_SET_6_Addr_HL'Access;
      Table (16#F7#) := Bitwise.Execute_SET_6_A'Access;
      Table (16#F8#) := Bitwise.Execute_SET_7_B'Access;
      Table (16#F9#) := Bitwise.Execute_SET_7_C'Access;
      Table (16#FA#) := Bitwise.Execute_SET_7_D'Access;
      Table (16#FB#) := Bitwise.Execute_SET_7_E'Access;
      Table (16#FC#) := Bitwise.Execute_SET_7_H'Access;
      Table (16#FD#) := Bitwise.Execute_SET_7_L'Access;
      Table (16#FE#) := Bitwise.Execute_SET_7_Addr_HL'Access;
      Table (16#FF#) := Bitwise.Execute_SET_7_A'Access;
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

   procedure Execute
     (GB : in out Gade.GB.GB_Type) is
      Handler : Instruction_Handler;
      Opcode  : Byte;
      Prefix  : Opcode_Prefix := Main;
   begin
      Opcode := Gade.GB.Memory_Map.Read_Byte (GB, GB.CPU.PC);
      Gade.Dev.CPU.Cycle_Steps.Step_M_Cycle (GB.CPU);

      if Opcode = 16#CB# then
         Prefix := CB;
         Opcode := Gade.GB.Memory_Map.Read_Byte (GB, GB.CPU.PC + 1);
         Gade.Dev.CPU.Cycle_Steps.Step_M_Cycle (GB.CPU);
      end if;

      GB.CPU.Branch_Taken := False;

      case Prefix is
         when Main =>
            Handler := Main_Handler (Opcode);
            if Handler = null then
               raise Program_Error with
                 "missing "
                 & Prefix_Image (Prefix)
                 & " handler for opcode 0x"
                 & Hex_Byte (Opcode)
                 & " at PC=0x"
                 & Hex_Word (GB.CPU.PC);
            end if;

            GB.CPU.PC := GB.CPU.PC + 1;
         when CB =>
            Handler := CB_Handler (Opcode);
            if Handler = null then
               raise Program_Error with
                 "missing "
                 & Prefix_Image (Prefix)
                 & " handler for opcode 0x"
                 & Hex_Byte (Opcode)
                 & " at PC=0x"
                 & Hex_Word (GB.CPU.PC);
            end if;

            GB.CPU.PC := GB.CPU.PC + 2;
      end case;

      Handler.all (GB);
   end Execute;

   function Hex_Digit (Value : Natural) return Character is
   begin
      if Value < 10 then
         return Character'Val (Character'Pos ('0') + Value);
      else
         return Character'Val (Character'Pos ('A') + Value - 10);
      end if;
   end Hex_Digit;

   function Hex_Byte (Value : Byte) return String is
      Raw : constant Natural := Natural (Value);
   begin
      return
        [1 => Hex_Digit (Raw / 16),
         2 => Hex_Digit (Raw mod 16)];
   end Hex_Byte;

   function Hex_Word (Value : Word) return String is
      Raw : constant Natural := Natural (Value);
   begin
      return
        [1 => Hex_Digit ((Raw / 16#1000#) mod 16),
         2 => Hex_Digit ((Raw / 16#100#) mod 16),
         3 => Hex_Digit ((Raw / 16#10#) mod 16),
         4 => Hex_Digit (Raw mod 16)];
   end Hex_Word;

   function Prefix_Image (Prefix : Opcode_Prefix) return String is
   begin
      case Prefix is
         when Main =>
            return "main";
         when CB =>
            return "cb";
      end case;
   end Prefix_Image;

end Gade.Dev.CPU.Dispatch;
