package Gade.Dev.CPU.Instructions.Flow is
   package Instructions renames Gade.Dev.CPU.Instructions;

   generic
      Operation : Instructions.Flow_Op_Kind;
      Condition : Instructions.Jump_Condition_Kind := Instructions.JCOND_None;
      Target    : Instructions.Jump_Target_Kind := Instructions.JTARGET_Imm16;
      Vector    : Word := 0;
   procedure Execute_Flow
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_JR
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_JR_NZ
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_JR_Z
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_JR_NC
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_JR_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_JP
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_JP_NZ
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_JP_Z
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_JP_NC
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_JP_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_JP_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_CALL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_CALL_NZ
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_CALL_Z
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_CALL_NC
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_CALL_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RET_NZ
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RET_Z
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RET
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RET_NC
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RET_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RETI
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RST_00
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RST_08
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RST_10
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RST_18
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RST_20
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RST_28
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RST_30
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RST_38
     (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Instructions.Flow;
