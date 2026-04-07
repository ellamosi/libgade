package Gade.Dev.CPU.Instructions.Flow.Handlers is

   --  JR
   procedure JR is new Flow_Op (FLOW_JR);

   procedure JR_NZ is new Flow_Op (FLOW_JR, JCOND_NZ);
   procedure JR_Z is new Flow_Op (FLOW_JR, JCOND_Z);
   procedure JR_NC is new Flow_Op (FLOW_JR, JCOND_NC);
   procedure JR_C is new Flow_Op (FLOW_JR, JCOND_C);

   --  JP
   procedure JP is new Flow_Op (FLOW_JP);
   procedure JP_HL is new Flow_Op (FLOW_JP, JCOND_None, JTARGET_HL);

   procedure JP_NZ is new Flow_Op (FLOW_JP, JCOND_NZ);
   procedure JP_Z is new Flow_Op (FLOW_JP, JCOND_Z);
   procedure JP_NC is new Flow_Op (FLOW_JP, JCOND_NC);
   procedure JP_C is new Flow_Op (FLOW_JP, JCOND_C);

   --  CALL
   procedure CALL is new Flow_Op (FLOW_CALL);

   procedure CALL_NZ is new Flow_Op (FLOW_CALL, JCOND_NZ);
   procedure CALL_Z is new Flow_Op (FLOW_CALL, JCOND_Z);
   procedure CALL_NC is new Flow_Op (FLOW_CALL, JCOND_NC);
   procedure CALL_C is new Flow_Op (FLOW_CALL, JCOND_C);

   --  RET
   procedure RET is new Flow_Op (FLOW_RET);
   procedure RETI is new Flow_Op (FLOW_RETI);

   procedure RET_NZ is new Flow_Op (FLOW_RET, JCOND_NZ);
   procedure RET_Z is new Flow_Op (FLOW_RET, JCOND_Z);
   procedure RET_NC is new Flow_Op (FLOW_RET, JCOND_NC);
   procedure RET_C is new Flow_Op (FLOW_RET, JCOND_C);

   --  RST
   procedure RST_00 is new Flow_Op (FLOW_RST, JCOND_None, JTARGET_Imm16, 16#0000#);
   procedure RST_08 is new Flow_Op (FLOW_RST, JCOND_None, JTARGET_Imm16, 16#0008#);
   procedure RST_10 is new Flow_Op (FLOW_RST, JCOND_None, JTARGET_Imm16, 16#0010#);
   procedure RST_18 is new Flow_Op (FLOW_RST, JCOND_None, JTARGET_Imm16, 16#0018#);
   procedure RST_20 is new Flow_Op (FLOW_RST, JCOND_None, JTARGET_Imm16, 16#0020#);
   procedure RST_28 is new Flow_Op (FLOW_RST, JCOND_None, JTARGET_Imm16, 16#0028#);
   procedure RST_30 is new Flow_Op (FLOW_RST, JCOND_None, JTARGET_Imm16, 16#0030#);
   procedure RST_38 is new Flow_Op (FLOW_RST, JCOND_None, JTARGET_Imm16, 16#0038#);

end Gade.Dev.CPU.Instructions.Flow.Handlers;
