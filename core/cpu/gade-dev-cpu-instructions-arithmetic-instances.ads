with Gade.GB;

package Gade.Dev.CPU.Instructions.Arithmetic.Instances is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure ADD_A_B is new ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_B);

   procedure ADD_A_Addr_HL is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_Addr_HL);

   procedure ADC_A_C is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_C);

   procedure SUB_A_D is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_D);

   procedure SBC_A_E is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_E);

   procedure ADD_A_C is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_C);

   procedure ADD_A_D is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_D);

   procedure ADD_A_E is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_E);

   procedure ADD_A_H is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_H);

   procedure ADD_A_L is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_L);

   procedure ADD_A_A is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_A);

   procedure ADC_A_B is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_B);

   procedure ADC_A_D is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_D);

   procedure ADC_A_E is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_E);

   procedure ADC_A_H is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_H);

   procedure ADC_A_L is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_L);

   procedure ADC_A_Addr_HL is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_Addr_HL);

   procedure ADC_A_A is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_A);

   procedure SUB_A_B is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_B);

   procedure SUB_A_C is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_C);

   procedure SUB_A_E is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_E);

   procedure SUB_A_H is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_H);

   procedure SUB_A_L is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_L);

   procedure SUB_A_Addr_HL is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_Addr_HL);

   procedure SUB_A_A is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_A);

   procedure SBC_A_B is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_B);

   procedure SBC_A_C is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_C);

   procedure SBC_A_D is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_D);

   procedure SBC_A_H is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_H);

   procedure SBC_A_L is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_L);

   procedure SBC_A_Addr_HL is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_Addr_HL);

   procedure SBC_A_A is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_A);

   procedure ADD_A_Imm8 is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_Imm8);

   procedure ADC_A_Imm8 is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_Imm8);

   procedure SUB_A_Imm8 is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_Imm8);

   procedure SBC_A_Imm8 is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_Imm8);

   procedure ADD_HL_BC is new Gade.Dev.CPU.Instructions.Arithmetic.Add_HL
     (Source => Instructions.REG_BC);

   procedure ADD_HL_DE is new Gade.Dev.CPU.Instructions.Arithmetic.Add_HL
     (Source => Instructions.REG_DE);

   procedure ADD_HL_HL is new Gade.Dev.CPU.Instructions.Arithmetic.Add_HL
     (Source => Instructions.REG_HL);

   procedure ADD_HL_SP is new Gade.Dev.CPU.Instructions.Arithmetic.Add_HL
     (Source => Instructions.REG_SP);

   procedure INC_BC is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_BC);

   procedure DEC_BC is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_BC);

   procedure INC_DE is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_DE);

   procedure DEC_DE is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_DE);

   procedure INC_SP is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_SP);

   procedure INC_HL is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_HL);

   procedure DEC_HL is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_HL);

   procedure DEC_SP is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_SP);

   procedure INC_B is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_B);

   procedure DEC_B is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_B);

   procedure INC_C is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_C);

   procedure DEC_C is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_C);

   procedure INC_D is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_D);

   procedure DEC_D is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_D);

   procedure INC_E is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_E);

   procedure DEC_E is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_E);

   procedure INC_H is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_H);

   procedure DEC_H is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_H);

   procedure INC_L is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_L);

   procedure DEC_L is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_L);

   procedure INC_Addr_HL is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_Addr_HL);

   procedure DEC_Addr_HL is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_Addr_HL);

   procedure INC_A is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_A);

   procedure DEC_A is new Gade.Dev.CPU.Instructions.Arithmetic.Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_A);

   procedure ADD_SP_Imm8
     (GB : in out Gade.GB.GB_Type) renames Gade.Dev.CPU.Instructions.Arithmetic.ADD_SP_Imm8;

   procedure DAA
     (GB : in out Gade.GB.GB_Type) renames Gade.Dev.CPU.Instructions.Arithmetic.DAA;

end Gade.Dev.CPU.Instructions.Arithmetic.Instances;
