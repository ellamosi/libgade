limited with Gade.GB;
with Gade.Dev.CPU.Decode;

package Gade.Dev.CPU.Decoded_Instructions is

   procedure Execute
     (GB   : in out Gade.GB.GB_Type;
      Inst :        Gade.Dev.CPU.Decode.Decoded_Instruction);

end Gade.Dev.CPU.Decoded_Instructions;
