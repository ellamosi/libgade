with Gade.Dev.CPU.Decode; use Gade.Dev.CPU.Decode;

package Gade.Dev.CPU.Timing is

   function Cycles
     (Inst         : Decoded_Instruction;
      Branch_Taken : Boolean) return M_Cycle_Count;

end Gade.Dev.CPU.Timing;
