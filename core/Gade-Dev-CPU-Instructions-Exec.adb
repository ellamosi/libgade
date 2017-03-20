with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Integer_Text_IO;             use Ada.Integer_Text_IO;

with Gade.Dev.CPU.Instructions.Table; use Gade.Dev.CPU.Instructions.Table;
with Gade.GB;                         use Gade.GB;
with Gade.GB.Memory_Map;              use Gade.GB.Memory_Map;

with Gade.Dev.Interrupts;

package body Gade.Dev.CPU.Instructions.Exec is

   procedure Execute
     (CPU    : in out CPU_Context;
      GB     : in out Gade.GB.GB_Type;
      Cycles : out Natural) is
      Instruction_Addr : constant Word := GB.CPU.PC;
      Current_Table    : access constant Instruction_Array;
      Instruction      : access constant Instruction_Entry;
      Partial_Opcode   : Byte;
   begin
      Current_Table := Opcodes_Main.Entries'Access;
      -- Ada.Integer_Text_IO.Put(Integer(Instruction_Addr), Base => 16);
      loop
         Partial_Opcode := Read_Byte(GB, GB.CPU.PC);
         GB.CPU.PC := GB.CPU.PC + 1;

         Instruction := Current_Table(Partial_Opcode)'Access;
         if Instruction.Method /= Null then
            -- Put_Line(' ' & Instruction.Name.all);

            GB.CPU.Branch_Taken := False;
            Instruction.Method(GB); -- RUN INSTRUCTION

            if GB.CPU.Branch_Taken then
               Cycles := Instruction.Jump_Cycles;
            else
               Cycles := Instruction.Cycles;
            end if;
            exit;
         elsif Instruction.Extended_Table /= Null then
            Current_Table := Instruction.Extended_Table.Entries'Access;
         else
            Put_Line("Unrecognized opcode!");
            raise Program_Error;
         end if;
      end loop;
   end Execute;

end Gade.Dev.CPU.Instructions.Exec;
