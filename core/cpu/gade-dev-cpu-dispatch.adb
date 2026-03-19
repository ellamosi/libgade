with Gade.Dev.CPU.Cycle_Steps;
with Gade.GB.Memory_Map;

package body Gade.Dev.CPU.Dispatch is
   function Hex_Digit (Value : Natural) return Character;

   function Hex_Byte (Value : Byte) return String;

   function Hex_Word (Value : Word) return String;

   procedure Execute
     (GB : in out Gade.GB.GB_Type) is
      Handler : Instruction_Handler;
      Opcode  : Byte;
   begin
      Opcode := Gade.GB.Memory_Map.Read_Byte (GB, GB.CPU.PC);
      Gade.Dev.CPU.Cycle_Steps.Step_M_Cycle (GB.CPU);

      GB.CPU.Branch_Taken := False;
      Handler := Main_Table (Opcode);
      GB.CPU.PC := GB.CPU.PC + 1;

      Handler.all (GB);
   end Execute;

   function Hex_Digit (Value : Natural) return Character is
   begin
      if Value < 10 then
         return Character'Val (Character'Pos ('0') + Value);
      else
         return Character'Val (Character'Pos ('A') + Value - 10);
      end if;
   end Hex_Digit;

   function Hex_Byte (Value : Byte) return String is
      Raw : constant Natural := Natural (Value);
   begin
      return
        [1 => Hex_Digit (Raw / 16),
         2 => Hex_Digit (Raw mod 16)];
   end Hex_Byte;

   function Hex_Word (Value : Word) return String is
      Raw : constant Natural := Natural (Value);
   begin
      return
        [1 => Hex_Digit ((Raw / 16#1000#) mod 16),
         2 => Hex_Digit ((Raw / 16#100#) mod 16),
         3 => Hex_Digit ((Raw / 16#10#) mod 16),
         4 => Hex_Digit (Raw mod 16)];
   end Hex_Word;

   procedure Execute_Invalid_Main_Opcode
     (GB : in out Gade.GB.GB_Type) is
      Opcode : constant Byte := Gade.GB.Memory_Map.Read_Byte (GB, GB.CPU.PC - 1);
   begin
      raise Program_Error with
        "invalid main opcode 0x"
        & Hex_Byte (Opcode)
        & " at PC=0x"
        & Hex_Word (GB.CPU.PC - 1);
   end Execute_Invalid_Main_Opcode;

   procedure Execute_Main_CB_Prefix
     (GB : in out Gade.GB.GB_Type) is
      Handler : Instruction_Handler;
      Opcode  : Byte;
   begin
      Opcode := Gade.GB.Memory_Map.Read_Byte (GB, GB.CPU.PC);
      Gade.Dev.CPU.Cycle_Steps.Step_M_Cycle (GB.CPU);
      Handler := CB_Table (Opcode);
      GB.CPU.PC := GB.CPU.PC + 1;
      Handler.all (GB);
   end Execute_Main_CB_Prefix;

end Gade.Dev.CPU.Dispatch;
