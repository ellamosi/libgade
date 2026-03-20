package Gade.Dev.CPU.Instructions.Flow.Instances is
   procedure JR is new Flow_Op
     (Operation => FLOW_JR);

   procedure JR_NZ is new Flow_Op
     (Operation => FLOW_JR,
      Condition => JCOND_NZ);

   procedure JR_Z is new Flow_Op
     (Operation => FLOW_JR,
      Condition => JCOND_Z);

   procedure JR_NC is new Flow_Op
     (Operation => FLOW_JR,
      Condition => JCOND_NC);

   procedure JR_C is new Flow_Op
     (Operation => FLOW_JR,
      Condition => JCOND_C);

   procedure JP is new Flow_Op
     (Operation => FLOW_JP);

   procedure JP_NZ is new Flow_Op
     (Operation => FLOW_JP,
      Condition => JCOND_NZ);

   procedure JP_Z is new Flow_Op
     (Operation => FLOW_JP,
      Condition => JCOND_Z);

   procedure JP_NC is new Flow_Op
     (Operation => FLOW_JP,
      Condition => JCOND_NC);

   procedure JP_C is new Flow_Op
     (Operation => FLOW_JP,
      Condition => JCOND_C);

   procedure JP_HL is new Flow_Op
     (Operation => FLOW_JP,
      Target    => JTARGET_HL);

   procedure CALL is new Flow_Op
     (Operation => FLOW_CALL);

   procedure CALL_NZ is new Flow_Op
     (Operation => FLOW_CALL,
      Condition => JCOND_NZ);

   procedure CALL_Z is new Flow_Op
     (Operation => FLOW_CALL,
      Condition => JCOND_Z);

   procedure CALL_NC is new Flow_Op
     (Operation => FLOW_CALL,
      Condition => JCOND_NC);

   procedure CALL_C is new Flow_Op
     (Operation => FLOW_CALL,
      Condition => JCOND_C);

   procedure RET_NZ is new Flow_Op
     (Operation => FLOW_RET,
      Condition => JCOND_NZ);

   procedure RET_Z is new Flow_Op
     (Operation => FLOW_RET,
      Condition => JCOND_Z);

   procedure RET is new Flow_Op
     (Operation => FLOW_RET);

   procedure RET_NC is new Flow_Op
     (Operation => FLOW_RET,
      Condition => JCOND_NC);

   procedure RET_C is new Flow_Op
     (Operation => FLOW_RET,
      Condition => JCOND_C);

   procedure RETI is new Flow_Op
     (Operation => FLOW_RETI);

   procedure RST_00 is new Flow_Op
     (Operation => FLOW_RST,
      Vector    => 16#0000#);

   procedure RST_08 is new Flow_Op
     (Operation => FLOW_RST,
      Vector    => 16#0008#);

   procedure RST_10 is new Flow_Op
     (Operation => FLOW_RST,
      Vector    => 16#0010#);

   procedure RST_18 is new Flow_Op
     (Operation => FLOW_RST,
      Vector    => 16#0018#);

   procedure RST_20 is new Flow_Op
     (Operation => FLOW_RST,
      Vector    => 16#0020#);

   procedure RST_28 is new Flow_Op
     (Operation => FLOW_RST,
      Vector    => 16#0028#);

   procedure RST_30 is new Flow_Op
     (Operation => FLOW_RST,
      Vector    => 16#0030#);

   procedure RST_38 is new Flow_Op
     (Operation => FLOW_RST,
      Vector    => 16#0038#);

end Gade.Dev.CPU.Instructions.Flow.Instances;
