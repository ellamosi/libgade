with Gade.GB;

package Gade.Dev.CPU.Instructions.Flow is
   package Instructions renames Gade.Dev.CPU.Instructions;

   generic
      Operation : Instructions.Flow_Op_Kind;
      Condition : Instructions.Jump_Condition_Kind := Instructions.JCOND_None;
      Target : Instructions.Jump_Target_Kind := Instructions.JTARGET_Imm16;
      Vector : Word := 0;
   procedure Flow_Op (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Instructions.Flow;
