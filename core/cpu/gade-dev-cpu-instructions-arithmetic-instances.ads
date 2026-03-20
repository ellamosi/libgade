with Gade.GB; use Gade.GB;

package Gade.Dev.CPU.Instructions.Arithmetic.Instances is
   package Arithmetic renames Gade.Dev.CPU.Instructions.Arithmetic;

   --  ADD
   procedure ADD_A_A is new ALU_A_Source (ALU_ADD, SRC_A);
   procedure ADD_A_B is new ALU_A_Source (ALU_ADD, SRC_B);
   procedure ADD_A_C is new ALU_A_Source (ALU_ADD, SRC_C);
   procedure ADD_A_D is new ALU_A_Source (ALU_ADD, SRC_D);
   procedure ADD_A_E is new ALU_A_Source (ALU_ADD, SRC_E);
   procedure ADD_A_H is new ALU_A_Source (ALU_ADD, SRC_H);
   procedure ADD_A_L is new ALU_A_Source (ALU_ADD, SRC_L);
   procedure ADD_A_Addr_HL is new ALU_A_Source (ALU_ADD, SRC_Addr_HL);
   procedure ADD_A_Imm8 is new ALU_A_Source (ALU_ADD, SRC_Imm8);

   --  ADC
   procedure ADC_A_A is new ALU_A_Source (ALU_ADC, SRC_A);
   procedure ADC_A_B is new ALU_A_Source (ALU_ADC, SRC_B);
   procedure ADC_A_C is new ALU_A_Source (ALU_ADC, SRC_C);
   procedure ADC_A_D is new ALU_A_Source (ALU_ADC, SRC_D);
   procedure ADC_A_E is new ALU_A_Source (ALU_ADC, SRC_E);
   procedure ADC_A_H is new ALU_A_Source (ALU_ADC, SRC_H);
   procedure ADC_A_L is new ALU_A_Source (ALU_ADC, SRC_L);
   procedure ADC_A_Addr_HL is new ALU_A_Source (ALU_ADC, SRC_Addr_HL);
   procedure ADC_A_Imm8 is new ALU_A_Source (ALU_ADC, SRC_Imm8);

   --  SUB
   procedure SUB_A_A is new ALU_A_Source (ALU_SUB, SRC_A);
   procedure SUB_A_B is new ALU_A_Source (ALU_SUB, SRC_B);
   procedure SUB_A_C is new ALU_A_Source (ALU_SUB, SRC_C);
   procedure SUB_A_D is new ALU_A_Source (ALU_SUB, SRC_D);
   procedure SUB_A_E is new ALU_A_Source (ALU_SUB, SRC_E);
   procedure SUB_A_H is new ALU_A_Source (ALU_SUB, SRC_H);
   procedure SUB_A_L is new ALU_A_Source (ALU_SUB, SRC_L);
   procedure SUB_A_Addr_HL is new ALU_A_Source (ALU_SUB, SRC_Addr_HL);
   procedure SUB_A_Imm8 is new ALU_A_Source (ALU_SUB, SRC_Imm8);

   --  SBC
   procedure SBC_A_A is new ALU_A_Source (ALU_SBC, SRC_A);
   procedure SBC_A_B is new ALU_A_Source (ALU_SBC, SRC_B);
   procedure SBC_A_C is new ALU_A_Source (ALU_SBC, SRC_C);
   procedure SBC_A_D is new ALU_A_Source (ALU_SBC, SRC_D);
   procedure SBC_A_E is new ALU_A_Source (ALU_SBC, SRC_E);
   procedure SBC_A_H is new ALU_A_Source (ALU_SBC, SRC_H);
   procedure SBC_A_L is new ALU_A_Source (ALU_SBC, SRC_L);
   procedure SBC_A_Addr_HL is new ALU_A_Source (ALU_SBC, SRC_Addr_HL);
   procedure SBC_A_Imm8 is new ALU_A_Source (ALU_SBC, SRC_Imm8);

   --  ADD HL,*
   procedure ADD_HL_BC is new Add_HL (REG_BC);
   procedure ADD_HL_DE is new Add_HL (REG_DE);
   procedure ADD_HL_HL is new Add_HL (REG_HL);
   procedure ADD_HL_SP is new Add_HL (REG_SP);

   --  INC word
   procedure INC_BC is new Inc_Dec_Word (OP_INC, REG_BC);
   procedure INC_DE is new Inc_Dec_Word (OP_INC, REG_DE);
   procedure INC_HL is new Inc_Dec_Word (OP_INC, REG_HL);
   procedure INC_SP is new Inc_Dec_Word (OP_INC, REG_SP);

   --  DEC word
   procedure DEC_BC is new Inc_Dec_Word (OP_DEC, REG_BC);
   procedure DEC_DE is new Inc_Dec_Word (OP_DEC, REG_DE);
   procedure DEC_HL is new Inc_Dec_Word (OP_DEC, REG_HL);
   procedure DEC_SP is new Inc_Dec_Word (OP_DEC, REG_SP);

   --  INC byte
   procedure INC_A is new Inc_Dec_Byte (OP_INC, DST_A);
   procedure INC_B is new Inc_Dec_Byte (OP_INC, DST_B);
   procedure INC_C is new Inc_Dec_Byte (OP_INC, DST_C);
   procedure INC_D is new Inc_Dec_Byte (OP_INC, DST_D);
   procedure INC_E is new Inc_Dec_Byte (OP_INC, DST_E);
   procedure INC_H is new Inc_Dec_Byte (OP_INC, DST_H);
   procedure INC_L is new Inc_Dec_Byte (OP_INC, DST_L);
   procedure INC_Addr_HL is new Inc_Dec_Byte (OP_INC, DST_Addr_HL);

   --  DEC byte
   procedure DEC_A is new Inc_Dec_Byte (OP_DEC, DST_A);
   procedure DEC_B is new Inc_Dec_Byte (OP_DEC, DST_B);
   procedure DEC_C is new Inc_Dec_Byte (OP_DEC, DST_C);
   procedure DEC_D is new Inc_Dec_Byte (OP_DEC, DST_D);
   procedure DEC_E is new Inc_Dec_Byte (OP_DEC, DST_E);
   procedure DEC_H is new Inc_Dec_Byte (OP_DEC, DST_H);
   procedure DEC_L is new Inc_Dec_Byte (OP_DEC, DST_L);
   procedure DEC_Addr_HL is new Inc_Dec_Byte (OP_DEC, DST_Addr_HL);

   --  Special
   procedure ADD_SP_Imm8 (GB : in out GB_Type) renames Arithmetic.ADD_SP_Imm8;
   procedure DAA (GB : in out GB_Type) renames Arithmetic.DAA;

end Gade.Dev.CPU.Instructions.Arithmetic.Instances;
