with Gade.GB;

package Gade.Dev.CPU.Instructions.Arithmetic.Instances is
   package Arithmetic renames Gade.Dev.CPU.Instructions.Arithmetic;

   procedure ADD_A_B is new ALU_A_Source
     (Operation => ALU_ADD,
      Source    => SRC_B);

   procedure ADD_A_Addr_HL is new ALU_A_Source
     (Operation => ALU_ADD,
      Source    => SRC_Addr_HL);

   procedure ADC_A_C is new ALU_A_Source
     (Operation => ALU_ADC,
      Source    => SRC_C);

   procedure SUB_A_D is new ALU_A_Source
     (Operation => ALU_SUB,
      Source    => SRC_D);

   procedure SBC_A_E is new ALU_A_Source
     (Operation => ALU_SBC,
      Source    => SRC_E);

   procedure ADD_A_C is new ALU_A_Source
     (Operation => ALU_ADD,
      Source    => SRC_C);

   procedure ADD_A_D is new ALU_A_Source
     (Operation => ALU_ADD,
      Source    => SRC_D);

   procedure ADD_A_E is new ALU_A_Source
     (Operation => ALU_ADD,
      Source    => SRC_E);

   procedure ADD_A_H is new ALU_A_Source
     (Operation => ALU_ADD,
      Source    => SRC_H);

   procedure ADD_A_L is new ALU_A_Source
     (Operation => ALU_ADD,
      Source    => SRC_L);

   procedure ADD_A_A is new ALU_A_Source
     (Operation => ALU_ADD,
      Source    => SRC_A);

   procedure ADC_A_B is new ALU_A_Source
     (Operation => ALU_ADC,
      Source    => SRC_B);

   procedure ADC_A_D is new ALU_A_Source
     (Operation => ALU_ADC,
      Source    => SRC_D);

   procedure ADC_A_E is new ALU_A_Source
     (Operation => ALU_ADC,
      Source    => SRC_E);

   procedure ADC_A_H is new ALU_A_Source
     (Operation => ALU_ADC,
      Source    => SRC_H);

   procedure ADC_A_L is new ALU_A_Source
     (Operation => ALU_ADC,
      Source    => SRC_L);

   procedure ADC_A_Addr_HL is new ALU_A_Source
     (Operation => ALU_ADC,
      Source    => SRC_Addr_HL);

   procedure ADC_A_A is new ALU_A_Source
     (Operation => ALU_ADC,
      Source    => SRC_A);

   procedure SUB_A_B is new ALU_A_Source
     (Operation => ALU_SUB,
      Source    => SRC_B);

   procedure SUB_A_C is new ALU_A_Source
     (Operation => ALU_SUB,
      Source    => SRC_C);

   procedure SUB_A_E is new ALU_A_Source
     (Operation => ALU_SUB,
      Source    => SRC_E);

   procedure SUB_A_H is new ALU_A_Source
     (Operation => ALU_SUB,
      Source    => SRC_H);

   procedure SUB_A_L is new ALU_A_Source
     (Operation => ALU_SUB,
      Source    => SRC_L);

   procedure SUB_A_Addr_HL is new ALU_A_Source
     (Operation => ALU_SUB,
      Source    => SRC_Addr_HL);

   procedure SUB_A_A is new ALU_A_Source
     (Operation => ALU_SUB,
      Source    => SRC_A);

   procedure SBC_A_B is new ALU_A_Source
     (Operation => ALU_SBC,
      Source    => SRC_B);

   procedure SBC_A_C is new ALU_A_Source
     (Operation => ALU_SBC,
      Source    => SRC_C);

   procedure SBC_A_D is new ALU_A_Source
     (Operation => ALU_SBC,
      Source    => SRC_D);

   procedure SBC_A_H is new ALU_A_Source
     (Operation => ALU_SBC,
      Source    => SRC_H);

   procedure SBC_A_L is new ALU_A_Source
     (Operation => ALU_SBC,
      Source    => SRC_L);

   procedure SBC_A_Addr_HL is new ALU_A_Source
     (Operation => ALU_SBC,
      Source    => SRC_Addr_HL);

   procedure SBC_A_A is new ALU_A_Source
     (Operation => ALU_SBC,
      Source    => SRC_A);

   procedure ADD_A_Imm8 is new ALU_A_Source
     (Operation => ALU_ADD,
      Source    => SRC_Imm8);

   procedure ADC_A_Imm8 is new ALU_A_Source
     (Operation => ALU_ADC,
      Source    => SRC_Imm8);

   procedure SUB_A_Imm8 is new ALU_A_Source
     (Operation => ALU_SUB,
      Source    => SRC_Imm8);

   procedure SBC_A_Imm8 is new ALU_A_Source
     (Operation => ALU_SBC,
      Source    => SRC_Imm8);

   procedure ADD_HL_BC is new Add_HL
     (Source => REG_BC);

   procedure ADD_HL_DE is new Add_HL
     (Source => REG_DE);

   procedure ADD_HL_HL is new Add_HL
     (Source => REG_HL);

   procedure ADD_HL_SP is new Add_HL
     (Source => REG_SP);

   procedure INC_BC is new Inc_Dec_Word
     (Operation => OP_INC,
      Target    => REG_BC);

   procedure DEC_BC is new Inc_Dec_Word
     (Operation => OP_DEC,
      Target    => REG_BC);

   procedure INC_DE is new Inc_Dec_Word
     (Operation => OP_INC,
      Target    => REG_DE);

   procedure DEC_DE is new Inc_Dec_Word
     (Operation => OP_DEC,
      Target    => REG_DE);

   procedure INC_SP is new Inc_Dec_Word
     (Operation => OP_INC,
      Target    => REG_SP);

   procedure INC_HL is new Inc_Dec_Word
     (Operation => OP_INC,
      Target    => REG_HL);

   procedure DEC_HL is new Inc_Dec_Word
     (Operation => OP_DEC,
      Target    => REG_HL);

   procedure DEC_SP is new Inc_Dec_Word
     (Operation => OP_DEC,
      Target    => REG_SP);

   procedure INC_B is new Inc_Dec_Byte
     (Operation => OP_INC,
      Target    => DST_B);

   procedure DEC_B is new Inc_Dec_Byte
     (Operation => OP_DEC,
      Target    => DST_B);

   procedure INC_C is new Inc_Dec_Byte
     (Operation => OP_INC,
      Target    => DST_C);

   procedure DEC_C is new Inc_Dec_Byte
     (Operation => OP_DEC,
      Target    => DST_C);

   procedure INC_D is new Inc_Dec_Byte
     (Operation => OP_INC,
      Target    => DST_D);

   procedure DEC_D is new Inc_Dec_Byte
     (Operation => OP_DEC,
      Target    => DST_D);

   procedure INC_E is new Inc_Dec_Byte
     (Operation => OP_INC,
      Target    => DST_E);

   procedure DEC_E is new Inc_Dec_Byte
     (Operation => OP_DEC,
      Target    => DST_E);

   procedure INC_H is new Inc_Dec_Byte
     (Operation => OP_INC,
      Target    => DST_H);

   procedure DEC_H is new Inc_Dec_Byte
     (Operation => OP_DEC,
      Target    => DST_H);

   procedure INC_L is new Inc_Dec_Byte
     (Operation => OP_INC,
      Target    => DST_L);

   procedure DEC_L is new Inc_Dec_Byte
     (Operation => OP_DEC,
      Target    => DST_L);

   procedure INC_Addr_HL is new Inc_Dec_Byte
     (Operation => OP_INC,
      Target    => DST_Addr_HL);

   procedure DEC_Addr_HL is new Inc_Dec_Byte
     (Operation => OP_DEC,
      Target    => DST_Addr_HL);

   procedure INC_A is new Inc_Dec_Byte
     (Operation => OP_INC,
      Target    => DST_A);

   procedure DEC_A is new Inc_Dec_Byte
     (Operation => OP_DEC,
      Target    => DST_A);

   procedure ADD_SP_Imm8
     (GB : in out Gade.GB.GB_Type) renames Arithmetic.ADD_SP_Imm8;

   procedure DAA
     (GB : in out Gade.GB.GB_Type) renames Arithmetic.DAA;

end Gade.Dev.CPU.Instructions.Arithmetic.Instances;
