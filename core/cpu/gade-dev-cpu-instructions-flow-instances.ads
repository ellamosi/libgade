package Gade.Dev.CPU.Instructions.Flow.Instances is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure Execute_JR is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_JR);

   procedure Execute_JR_NZ is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_JR,
      Condition => Instructions.JCOND_NZ);

   procedure Execute_JR_Z is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_JR,
      Condition => Instructions.JCOND_Z);

   procedure Execute_JR_NC is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_JR,
      Condition => Instructions.JCOND_NC);

   procedure Execute_JR_C is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_JR,
      Condition => Instructions.JCOND_C);

   procedure Execute_JP is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_JP);

   procedure Execute_JP_NZ is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_JP,
      Condition => Instructions.JCOND_NZ);

   procedure Execute_JP_Z is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_JP,
      Condition => Instructions.JCOND_Z);

   procedure Execute_JP_NC is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_JP,
      Condition => Instructions.JCOND_NC);

   procedure Execute_JP_C is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_JP,
      Condition => Instructions.JCOND_C);

   procedure Execute_JP_HL is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_JP,
      Target    => Instructions.JTARGET_HL);

   procedure Execute_CALL is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_CALL);

   procedure Execute_CALL_NZ is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_CALL,
      Condition => Instructions.JCOND_NZ);

   procedure Execute_CALL_Z is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_CALL,
      Condition => Instructions.JCOND_Z);

   procedure Execute_CALL_NC is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_CALL,
      Condition => Instructions.JCOND_NC);

   procedure Execute_CALL_C is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_CALL,
      Condition => Instructions.JCOND_C);

   procedure Execute_RET_NZ is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RET,
      Condition => Instructions.JCOND_NZ);

   procedure Execute_RET_Z is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RET,
      Condition => Instructions.JCOND_Z);

   procedure Execute_RET is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RET);

   procedure Execute_RET_NC is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RET,
      Condition => Instructions.JCOND_NC);

   procedure Execute_RET_C is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RET,
      Condition => Instructions.JCOND_C);

   procedure Execute_RETI is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RETI);

   procedure Execute_RST_00 is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0000#);

   procedure Execute_RST_08 is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0008#);

   procedure Execute_RST_10 is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0010#);

   procedure Execute_RST_18 is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0018#);

   procedure Execute_RST_20 is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0020#);

   procedure Execute_RST_28 is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0028#);

   procedure Execute_RST_30 is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0030#);

   procedure Execute_RST_38 is new Gade.Dev.CPU.Instructions.Flow.Execute_Flow
     (Operation => Instructions.FLOW_RST,
      Vector    => 16#0038#);

end Gade.Dev.CPU.Instructions.Flow.Instances;
