with Gade.Dev.CPU.Generic_Dispatch_Prototype;
with Gade.Dev.CPU.Cycle_Steps;
with Gade.Dev.CPU.Decode; use Gade.Dev.CPU.Decode;
with Gade.GB;                           use Gade.GB;
with Gade.GB.Memory_Map;
with Ada.Text_IO;

package body Gade.Dev.CPU.Exec is

   Top_Up_Seen : array (Prefix_Kind, Byte) of Boolean := [others => [others => False]];

   function Hex_Digit (Value : Natural) return Character;

   function Hex_Byte (Value : Byte) return String;

   function Prefix_Image (Prefix : Prefix_Kind) return String;

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

   function Prefix_Image (Prefix : Prefix_Kind) return String is
   begin
      case Prefix is
         when Main =>
            return "main";
         when CB =>
            return "cb";
      end case;
   end Prefix_Image;

   procedure Execute
     (CPU    : in out CPU_Context;
      GB     : in out Gade.GB.GB_Type;
      Cycles : out M_Cycle_Count) is
      pragma Unreferenced (CPU);
      Expected_Cycles : M_Cycle_Count;
      Opcode          : Byte := Gade.GB.Memory_Map.Read_Byte (GB, GB.CPU.PC);
      Prefix          : Prefix_Kind := Main;
   begin
      if Opcode = 16#CB# then
         Prefix := CB;
         Opcode := Gade.GB.Memory_Map.Read_Byte (GB, GB.CPU.PC + 1);
      end if;

      Gade.Dev.CPU.Cycle_Steps.Reset (GB.CPU);
      Gade.Dev.CPU.Generic_Dispatch_Prototype.Execute (GB, Expected_Cycles);
      Cycles := Gade.Dev.CPU.Cycle_Steps.Consumed_Cycles (GB.CPU);

      if Cycles > Expected_Cycles then
         raise Program_Error with
           "cycle mismatch: stepped="
           & M_Cycle_Count'Image (Cycles)
           & " expected="
           & M_Cycle_Count'Image (Expected_Cycles);
      elsif Cycles < Expected_Cycles then
         if not Top_Up_Seen (Prefix, Opcode) then
            Top_Up_Seen (Prefix, Opcode) := True;
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Timing top-up "
               & Prefix_Image (Prefix)
               & " opcode=0x"
               & Hex_Byte (Opcode)
               & " stepped="
               & M_Cycle_Count'Image (Cycles)
               & " expected="
               & M_Cycle_Count'Image (Expected_Cycles));
         end if;
         Gade.Dev.CPU.Cycle_Steps.Step_M_Cycle (GB.CPU, Expected_Cycles - Cycles);
         Cycles := Expected_Cycles;
      end if;
   end Execute;

end Gade.Dev.CPU.Exec;
