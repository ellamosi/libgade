with Gade.Dev.CPU.Cycle_Steps;
with Gade.Dev.CPU.Generic_Handlers;
with Gade.Dev.CPU.Generic_Instruction_Definitions;
with Gade.GB.Memory_Map;

package body Gade.Dev.CPU.Generic_Dispatch_Prototype is
   package Definitions renames Gade.Dev.CPU.Generic_Instruction_Definitions;
   package Handlers renames Gade.Dev.CPU.Generic_Handlers;

   type Opcode_Prefix is (Main_Prefix, CB_Prefix);

   type Handler_Table is array (Byte) of Instruction_Handler;

   function Build_Main_Table return Handler_Table;

   function Build_CB_Table return Handler_Table;

   function Hex_Digit (Value : Natural) return Character;

   function Hex_Byte (Value : Byte) return String;

   function Hex_Word (Value : Word) return String;

   function Prefix_Image (Prefix : Opcode_Prefix) return String;

   procedure Execute_RES_0_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_B);
   procedure Execute_RES_0_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_C);
   procedure Execute_RES_0_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_D);
   procedure Execute_RES_0_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_E);
   procedure Execute_RES_0_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_H);
   procedure Execute_RES_0_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_L);
   procedure Execute_RES_0_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_0_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_A);
   procedure Execute_RES_1_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_B);
   procedure Execute_RES_1_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_C);
   procedure Execute_RES_1_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_D);
   procedure Execute_RES_1_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_E);
   procedure Execute_RES_1_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_H);
   procedure Execute_RES_1_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_L);
   procedure Execute_RES_1_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_1_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_A);
   procedure Execute_RES_2_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_B);
   procedure Execute_RES_2_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_C);
   procedure Execute_RES_2_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_D);
   procedure Execute_RES_2_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_E);
   procedure Execute_RES_2_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_H);
   procedure Execute_RES_2_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_L);
   procedure Execute_RES_2_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_2_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_A);
   procedure Execute_RES_3_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_B);
   procedure Execute_RES_3_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_C);
   procedure Execute_RES_3_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_D);
   procedure Execute_RES_3_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_E);
   procedure Execute_RES_3_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_H);
   procedure Execute_RES_3_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_L);
   procedure Execute_RES_3_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_3_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_A);
   procedure Execute_RES_4_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_B);
   procedure Execute_RES_4_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_C);
   procedure Execute_RES_4_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_D);
   procedure Execute_RES_4_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_E);
   procedure Execute_RES_4_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_H);
   procedure Execute_RES_4_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_L);
   procedure Execute_RES_4_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_4_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_A);
   procedure Execute_RES_5_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_B);
   procedure Execute_RES_5_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_C);
   procedure Execute_RES_5_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_D);
   procedure Execute_RES_5_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_E);
   procedure Execute_RES_5_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_H);
   procedure Execute_RES_5_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_L);
   procedure Execute_RES_5_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_5_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_A);
   procedure Execute_RES_6_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_B);
   procedure Execute_RES_6_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_C);
   procedure Execute_RES_6_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_D);
   procedure Execute_RES_6_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_E);
   procedure Execute_RES_6_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_H);
   procedure Execute_RES_6_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_L);
   procedure Execute_RES_6_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_6_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_A);
   procedure Execute_RES_7_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_B);
   procedure Execute_RES_7_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_C);
   procedure Execute_RES_7_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_D);
   procedure Execute_RES_7_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_E);
   procedure Execute_RES_7_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_H);
   procedure Execute_RES_7_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_L);
   procedure Execute_RES_7_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_7_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_A);
   procedure Execute_SET_0_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_B);
   procedure Execute_SET_0_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_C);
   procedure Execute_SET_0_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_D);
   procedure Execute_SET_0_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_E);
   procedure Execute_SET_0_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_H);
   procedure Execute_SET_0_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_L);
   procedure Execute_SET_0_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_0_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_A);
   procedure Execute_SET_1_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_B);
   procedure Execute_SET_1_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_C);
   procedure Execute_SET_1_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_D);
   procedure Execute_SET_1_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_E);
   procedure Execute_SET_1_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_H);
   procedure Execute_SET_1_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_L);
   procedure Execute_SET_1_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_1_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_A);
   procedure Execute_SET_2_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_B);
   procedure Execute_SET_2_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_C);
   procedure Execute_SET_2_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_D);
   procedure Execute_SET_2_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_E);
   procedure Execute_SET_2_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_H);
   procedure Execute_SET_2_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_L);
   procedure Execute_SET_2_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_2_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_A);
   procedure Execute_SET_3_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_B);
   procedure Execute_SET_3_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_C);
   procedure Execute_SET_3_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_D);
   procedure Execute_SET_3_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_E);
   procedure Execute_SET_3_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_H);
   procedure Execute_SET_3_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_L);
   procedure Execute_SET_3_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_3_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_A);
   procedure Execute_SET_4_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_B);
   procedure Execute_SET_4_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_C);
   procedure Execute_SET_4_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_D);
   procedure Execute_SET_4_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_E);
   procedure Execute_SET_4_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_H);
   procedure Execute_SET_4_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_L);
   procedure Execute_SET_4_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_4_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_A);
   procedure Execute_SET_5_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_B);
   procedure Execute_SET_5_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_C);
   procedure Execute_SET_5_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_D);
   procedure Execute_SET_5_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_E);
   procedure Execute_SET_5_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_H);
   procedure Execute_SET_5_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_L);
   procedure Execute_SET_5_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_5_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_A);
   procedure Execute_SET_6_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_B);
   procedure Execute_SET_6_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_C);
   procedure Execute_SET_6_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_D);
   procedure Execute_SET_6_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_E);
   procedure Execute_SET_6_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_H);
   procedure Execute_SET_6_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_L);
   procedure Execute_SET_6_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_6_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_A);
   procedure Execute_SET_7_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_B);
   procedure Execute_SET_7_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_C);
   procedure Execute_SET_7_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_D);
   procedure Execute_SET_7_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_E);
   procedure Execute_SET_7_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_H);
   procedure Execute_SET_7_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_L);
   procedure Execute_SET_7_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_7_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_A);

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
      Table (16#F6#) := Definitions.Execute_OR_A_Imm8'Access;
      Table (16#FB#) := Definitions.Execute_EI'Access;
      Table (16#FE#) := Definitions.Execute_CP_A_Imm8'Access;
      return Table;
   end Build_Main_Table;

   function Build_CB_Table return Handler_Table is
      Table : Handler_Table := [others => null];
   begin
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
      Table (16#40#) := Definitions.Execute_BIT_0_B'Access;
      Table (16#41#) := Definitions.Execute_BIT_0_C'Access;
      Table (16#42#) := Definitions.Execute_BIT_0_D'Access;
      Table (16#43#) := Definitions.Execute_BIT_0_E'Access;
      Table (16#44#) := Definitions.Execute_BIT_0_H'Access;
      Table (16#45#) := Definitions.Execute_BIT_0_L'Access;
      Table (16#46#) := Definitions.Execute_BIT_0_Addr_HL'Access;
      Table (16#47#) := Definitions.Execute_BIT_0_A'Access;
      Table (16#48#) := Definitions.Execute_BIT_1_B'Access;
      Table (16#49#) := Definitions.Execute_BIT_1_C'Access;
      Table (16#4A#) := Definitions.Execute_BIT_1_D'Access;
      Table (16#4B#) := Definitions.Execute_BIT_1_E'Access;
      Table (16#4C#) := Definitions.Execute_BIT_1_H'Access;
      Table (16#4D#) := Definitions.Execute_BIT_1_L'Access;
      Table (16#4E#) := Definitions.Execute_BIT_1_Addr_HL'Access;
      Table (16#4F#) := Definitions.Execute_BIT_1_A'Access;
      Table (16#50#) := Definitions.Execute_BIT_2_B'Access;
      Table (16#51#) := Definitions.Execute_BIT_2_C'Access;
      Table (16#52#) := Definitions.Execute_BIT_2_D'Access;
      Table (16#53#) := Definitions.Execute_BIT_2_E'Access;
      Table (16#54#) := Definitions.Execute_BIT_2_H'Access;
      Table (16#55#) := Definitions.Execute_BIT_2_L'Access;
      Table (16#56#) := Definitions.Execute_BIT_2_Addr_HL'Access;
      Table (16#57#) := Definitions.Execute_BIT_2_A'Access;
      Table (16#58#) := Definitions.Execute_BIT_3_B'Access;
      Table (16#59#) := Definitions.Execute_BIT_3_C'Access;
      Table (16#5A#) := Definitions.Execute_BIT_3_D'Access;
      Table (16#5B#) := Definitions.Execute_BIT_3_E'Access;
      Table (16#5C#) := Definitions.Execute_BIT_3_H'Access;
      Table (16#5D#) := Definitions.Execute_BIT_3_L'Access;
      Table (16#5E#) := Definitions.Execute_BIT_3_Addr_HL'Access;
      Table (16#5F#) := Definitions.Execute_BIT_3_A'Access;
      Table (16#60#) := Definitions.Execute_BIT_4_B'Access;
      Table (16#61#) := Definitions.Execute_BIT_4_C'Access;
      Table (16#62#) := Definitions.Execute_BIT_4_D'Access;
      Table (16#63#) := Definitions.Execute_BIT_4_E'Access;
      Table (16#64#) := Definitions.Execute_BIT_4_H'Access;
      Table (16#65#) := Definitions.Execute_BIT_4_L'Access;
      Table (16#66#) := Definitions.Execute_BIT_4_Addr_HL'Access;
      Table (16#67#) := Definitions.Execute_BIT_4_A'Access;
      Table (16#68#) := Definitions.Execute_BIT_5_B'Access;
      Table (16#69#) := Definitions.Execute_BIT_5_C'Access;
      Table (16#6A#) := Definitions.Execute_BIT_5_D'Access;
      Table (16#6B#) := Definitions.Execute_BIT_5_E'Access;
      Table (16#6C#) := Definitions.Execute_BIT_5_H'Access;
      Table (16#6D#) := Definitions.Execute_BIT_5_L'Access;
      Table (16#6E#) := Definitions.Execute_BIT_5_Addr_HL'Access;
      Table (16#6F#) := Definitions.Execute_BIT_5_A'Access;
      Table (16#70#) := Definitions.Execute_BIT_6_B'Access;
      Table (16#71#) := Definitions.Execute_BIT_6_C'Access;
      Table (16#72#) := Definitions.Execute_BIT_6_D'Access;
      Table (16#73#) := Definitions.Execute_BIT_6_E'Access;
      Table (16#74#) := Definitions.Execute_BIT_6_H'Access;
      Table (16#75#) := Definitions.Execute_BIT_6_L'Access;
      Table (16#76#) := Definitions.Execute_BIT_6_Addr_HL'Access;
      Table (16#77#) := Definitions.Execute_BIT_6_A'Access;
      Table (16#78#) := Definitions.Execute_BIT_7_B'Access;
      Table (16#79#) := Definitions.Execute_BIT_7_C'Access;
      Table (16#7A#) := Definitions.Execute_BIT_7_D'Access;
      Table (16#7B#) := Definitions.Execute_BIT_7_E'Access;
      Table (16#7C#) := Definitions.Execute_BIT_7_H'Access;
      Table (16#7D#) := Definitions.Execute_BIT_7_L'Access;
      Table (16#7E#) := Definitions.Execute_BIT_7_Addr_HL'Access;
      Table (16#7F#) := Definitions.Execute_BIT_7_A'Access;
      Table (16#80#) := Execute_RES_0_B'Access;
      Table (16#81#) := Execute_RES_0_C'Access;
      Table (16#82#) := Execute_RES_0_D'Access;
      Table (16#83#) := Execute_RES_0_E'Access;
      Table (16#84#) := Execute_RES_0_H'Access;
      Table (16#85#) := Execute_RES_0_L'Access;
      Table (16#86#) := Execute_RES_0_Addr_HL'Access;
      Table (16#87#) := Execute_RES_0_A'Access;
      Table (16#88#) := Execute_RES_1_B'Access;
      Table (16#89#) := Execute_RES_1_C'Access;
      Table (16#8A#) := Execute_RES_1_D'Access;
      Table (16#8B#) := Execute_RES_1_E'Access;
      Table (16#8C#) := Execute_RES_1_H'Access;
      Table (16#8D#) := Execute_RES_1_L'Access;
      Table (16#8E#) := Execute_RES_1_Addr_HL'Access;
      Table (16#8F#) := Execute_RES_1_A'Access;
      Table (16#90#) := Execute_RES_2_B'Access;
      Table (16#91#) := Execute_RES_2_C'Access;
      Table (16#92#) := Execute_RES_2_D'Access;
      Table (16#93#) := Execute_RES_2_E'Access;
      Table (16#94#) := Execute_RES_2_H'Access;
      Table (16#95#) := Execute_RES_2_L'Access;
      Table (16#96#) := Execute_RES_2_Addr_HL'Access;
      Table (16#97#) := Execute_RES_2_A'Access;
      Table (16#98#) := Execute_RES_3_B'Access;
      Table (16#99#) := Execute_RES_3_C'Access;
      Table (16#9A#) := Execute_RES_3_D'Access;
      Table (16#9B#) := Execute_RES_3_E'Access;
      Table (16#9C#) := Execute_RES_3_H'Access;
      Table (16#9D#) := Execute_RES_3_L'Access;
      Table (16#9E#) := Execute_RES_3_Addr_HL'Access;
      Table (16#9F#) := Execute_RES_3_A'Access;
      Table (16#A0#) := Execute_RES_4_B'Access;
      Table (16#A1#) := Execute_RES_4_C'Access;
      Table (16#A2#) := Execute_RES_4_D'Access;
      Table (16#A3#) := Execute_RES_4_E'Access;
      Table (16#A4#) := Execute_RES_4_H'Access;
      Table (16#A5#) := Execute_RES_4_L'Access;
      Table (16#A6#) := Execute_RES_4_Addr_HL'Access;
      Table (16#A7#) := Execute_RES_4_A'Access;
      Table (16#A8#) := Execute_RES_5_B'Access;
      Table (16#A9#) := Execute_RES_5_C'Access;
      Table (16#AA#) := Execute_RES_5_D'Access;
      Table (16#AB#) := Execute_RES_5_E'Access;
      Table (16#AC#) := Execute_RES_5_H'Access;
      Table (16#AD#) := Execute_RES_5_L'Access;
      Table (16#AE#) := Execute_RES_5_Addr_HL'Access;
      Table (16#AF#) := Execute_RES_5_A'Access;
      Table (16#B0#) := Execute_RES_6_B'Access;
      Table (16#B1#) := Execute_RES_6_C'Access;
      Table (16#B2#) := Execute_RES_6_D'Access;
      Table (16#B3#) := Execute_RES_6_E'Access;
      Table (16#B4#) := Execute_RES_6_H'Access;
      Table (16#B5#) := Execute_RES_6_L'Access;
      Table (16#B6#) := Execute_RES_6_Addr_HL'Access;
      Table (16#B7#) := Execute_RES_6_A'Access;
      Table (16#B8#) := Execute_RES_7_B'Access;
      Table (16#B9#) := Execute_RES_7_C'Access;
      Table (16#BA#) := Execute_RES_7_D'Access;
      Table (16#BB#) := Execute_RES_7_E'Access;
      Table (16#BC#) := Execute_RES_7_H'Access;
      Table (16#BD#) := Execute_RES_7_L'Access;
      Table (16#BE#) := Execute_RES_7_Addr_HL'Access;
      Table (16#BF#) := Execute_RES_7_A'Access;
      Table (16#C0#) := Execute_SET_0_B'Access;
      Table (16#C1#) := Execute_SET_0_C'Access;
      Table (16#C2#) := Execute_SET_0_D'Access;
      Table (16#C3#) := Execute_SET_0_E'Access;
      Table (16#C4#) := Execute_SET_0_H'Access;
      Table (16#C5#) := Execute_SET_0_L'Access;
      Table (16#C6#) := Execute_SET_0_Addr_HL'Access;
      Table (16#C7#) := Execute_SET_0_A'Access;
      Table (16#C8#) := Execute_SET_1_B'Access;
      Table (16#C9#) := Execute_SET_1_C'Access;
      Table (16#CA#) := Execute_SET_1_D'Access;
      Table (16#CB#) := Execute_SET_1_E'Access;
      Table (16#CC#) := Execute_SET_1_H'Access;
      Table (16#CD#) := Execute_SET_1_L'Access;
      Table (16#CE#) := Execute_SET_1_Addr_HL'Access;
      Table (16#CF#) := Execute_SET_1_A'Access;
      Table (16#D0#) := Execute_SET_2_B'Access;
      Table (16#D1#) := Execute_SET_2_C'Access;
      Table (16#D2#) := Execute_SET_2_D'Access;
      Table (16#D3#) := Execute_SET_2_E'Access;
      Table (16#D4#) := Execute_SET_2_H'Access;
      Table (16#D5#) := Execute_SET_2_L'Access;
      Table (16#D6#) := Execute_SET_2_Addr_HL'Access;
      Table (16#D7#) := Execute_SET_2_A'Access;
      Table (16#D8#) := Execute_SET_3_B'Access;
      Table (16#D9#) := Execute_SET_3_C'Access;
      Table (16#DA#) := Execute_SET_3_D'Access;
      Table (16#DB#) := Execute_SET_3_E'Access;
      Table (16#DC#) := Execute_SET_3_H'Access;
      Table (16#DD#) := Execute_SET_3_L'Access;
      Table (16#DE#) := Execute_SET_3_Addr_HL'Access;
      Table (16#DF#) := Execute_SET_3_A'Access;
      Table (16#E0#) := Execute_SET_4_B'Access;
      Table (16#E1#) := Execute_SET_4_C'Access;
      Table (16#E2#) := Execute_SET_4_D'Access;
      Table (16#E3#) := Execute_SET_4_E'Access;
      Table (16#E4#) := Execute_SET_4_H'Access;
      Table (16#E5#) := Execute_SET_4_L'Access;
      Table (16#E6#) := Execute_SET_4_Addr_HL'Access;
      Table (16#E7#) := Execute_SET_4_A'Access;
      Table (16#E8#) := Execute_SET_5_B'Access;
      Table (16#E9#) := Execute_SET_5_C'Access;
      Table (16#EA#) := Execute_SET_5_D'Access;
      Table (16#EB#) := Execute_SET_5_E'Access;
      Table (16#EC#) := Execute_SET_5_H'Access;
      Table (16#ED#) := Execute_SET_5_L'Access;
      Table (16#EE#) := Execute_SET_5_Addr_HL'Access;
      Table (16#EF#) := Execute_SET_5_A'Access;
      Table (16#F0#) := Execute_SET_6_B'Access;
      Table (16#F1#) := Execute_SET_6_C'Access;
      Table (16#F2#) := Execute_SET_6_D'Access;
      Table (16#F3#) := Execute_SET_6_E'Access;
      Table (16#F4#) := Execute_SET_6_H'Access;
      Table (16#F5#) := Execute_SET_6_L'Access;
      Table (16#F6#) := Execute_SET_6_Addr_HL'Access;
      Table (16#F7#) := Execute_SET_6_A'Access;
      Table (16#F8#) := Execute_SET_7_B'Access;
      Table (16#F9#) := Execute_SET_7_C'Access;
      Table (16#FA#) := Execute_SET_7_D'Access;
      Table (16#FB#) := Execute_SET_7_E'Access;
      Table (16#FC#) := Execute_SET_7_H'Access;
      Table (16#FD#) := Execute_SET_7_L'Access;
      Table (16#FE#) := Execute_SET_7_Addr_HL'Access;
      Table (16#FF#) := Execute_SET_7_A'Access;
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
      Prefix  : Opcode_Prefix := Main_Prefix;
   begin
      Opcode := Gade.GB.Memory_Map.Read_Byte (GB, GB.CPU.PC);
      Gade.Dev.CPU.Cycle_Steps.Step_M_Cycle (GB.CPU);

      if Opcode = 16#CB# then
         Prefix := CB_Prefix;
         Opcode := Gade.GB.Memory_Map.Read_Byte (GB, GB.CPU.PC + 1);
         Gade.Dev.CPU.Cycle_Steps.Step_M_Cycle (GB.CPU);
      end if;

      GB.CPU.Branch_Taken := False;

      case Prefix is
         when Main_Prefix =>
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
         when CB_Prefix =>
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
         when Main_Prefix =>
            return "main";
         when CB_Prefix =>
            return "cb";
      end case;
   end Prefix_Image;

end Gade.Dev.CPU.Generic_Dispatch_Prototype;
