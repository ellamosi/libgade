with Gade.Dev.CPU.Decode; use Gade.Dev.CPU.Decode;

package Gade.Dev.CPU.Timing is

   function Total_Cycles
     (Prefix       : Prefix_Kind;
      Opcode       : Byte;
      Branch_Taken : Boolean) return M_Cycle_Count;

end Gade.Dev.CPU.Timing;
