with Gade.Dev.CPU.Instructions.Arithmetic;
with Gade.Dev.CPU.Instructions.Logic;
with Gade.GB.Memory_Map; use Gade.GB.Memory_Map;

package body Gade.Dev.CPU.Instructions is

   function Bus_Read_Byte (GB : in out Gade.GB.GB_Type; Address : Word) return Byte is
      Value : constant Byte := Read_Byte (GB, Address);
   begin
      GB.CPU.Stepped_Cycles := GB.CPU.Stepped_Cycles + 1;
      Gade.GB.Tick_M_Cycle (GB);
      return Value;
   end Bus_Read_Byte;

   procedure Bus_Write_Byte (GB : in out Gade.GB.GB_Type; Address : Word; Value : Byte) is
   begin
      Write_Byte (GB, Address, Value);
      GB.CPU.Stepped_Cycles := GB.CPU.Stepped_Cycles + 1;
      Gade.GB.Tick_M_Cycle (GB);
   end Bus_Write_Byte;

   procedure Internal_Cycle (GB : in out Gade.GB.GB_Type) is
   begin
      GB.CPU.Stepped_Cycles := GB.CPU.Stepped_Cycles + 1;
      Gade.GB.Tick_M_Cycle (GB);
   end Internal_Cycle;

   function Fetch_Source
     (GB : in out Gade.GB.GB_Type; Source : Byte_Source_Kind) return Byte is
   begin
      case Source is
         when SRC_A                             =>
            return GB.CPU.Regs.A;

         when SRC_B                             =>
            return GB.CPU.Regs.B;

         when SRC_C                             =>
            return GB.CPU.Regs.C;

         when SRC_D                             =>
            return GB.CPU.Regs.D;

         when SRC_E                             =>
            return GB.CPU.Regs.E;

         when SRC_H                             =>
            return GB.CPU.Regs.H;

         when SRC_L                             =>
            return GB.CPU.Regs.L;

         when SRC_Addr_BC                       =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.BC);

         when SRC_Addr_DE                       =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.DE);

         when SRC_Addr_HL                       =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.HL);

         when SRC_Addr_HL_Inc | SRC_Addr_HL_Dec =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.HL);

         when SRC_Addr_Imm16                    =>
            return Bus_Read_Byte (GB, Fetch_Imm16 (GB));

         when SRC_High_Addr_C                   =>
            return Bus_Read_Byte (GB, 16#FF00# + Word (GB.CPU.Regs.C));

         when SRC_High_Addr_Imm8                =>
            return Bus_Read_Byte (GB, 16#FF00# + Word (Fetch_Imm8 (GB)));

         when SRC_Imm8                          =>
            return Fetch_Imm8 (GB);
      end case;
   end Fetch_Source;

   procedure Store_Target
     (GB : in out Gade.GB.GB_Type; Target : Byte_Target_Kind; Value : Byte) is
   begin
      case Target is
         when DST_A                             =>
            GB.CPU.Regs.A := Value;

         when DST_B                             =>
            GB.CPU.Regs.B := Value;

         when DST_C                             =>
            GB.CPU.Regs.C := Value;

         when DST_D                             =>
            GB.CPU.Regs.D := Value;

         when DST_E                             =>
            GB.CPU.Regs.E := Value;

         when DST_H                             =>
            GB.CPU.Regs.H := Value;

         when DST_L                             =>
            GB.CPU.Regs.L := Value;

         when DST_Addr_BC                       =>
            Bus_Write_Byte (GB, GB.CPU.Regs.BC, Value);

         when DST_Addr_DE                       =>
            Bus_Write_Byte (GB, GB.CPU.Regs.DE, Value);

         when DST_Addr_HL                       =>
            Bus_Write_Byte (GB, GB.CPU.Regs.HL, Value);

         when DST_Addr_HL_Inc | DST_Addr_HL_Dec =>
            Bus_Write_Byte (GB, GB.CPU.Regs.HL, Value);

         when DST_Addr_Imm16                    =>
            Bus_Write_Byte (GB, Fetch_Imm16 (GB), Value);

         when DST_High_Addr_C                   =>
            Bus_Write_Byte (GB, 16#FF00# + Word (GB.CPU.Regs.C), Value);

         when DST_High_Addr_Imm8                =>
            Bus_Write_Byte (GB, 16#FF00# + Word (Fetch_Imm8 (GB)), Value);
      end case;
   end Store_Target;

   function Load_Target
     (GB : in out Gade.GB.GB_Type; Target : Byte_Target_Kind) return Byte is
   begin
      case Target is
         when DST_A                                           =>
            return GB.CPU.Regs.A;

         when DST_B                                           =>
            return GB.CPU.Regs.B;

         when DST_C                                           =>
            return GB.CPU.Regs.C;

         when DST_D                                           =>
            return GB.CPU.Regs.D;

         when DST_E                                           =>
            return GB.CPU.Regs.E;

         when DST_H                                           =>
            return GB.CPU.Regs.H;

         when DST_L                                           =>
            return GB.CPU.Regs.L;

         when DST_Addr_BC                                     =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.BC);

         when DST_Addr_DE                                     =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.DE);

         when DST_Addr_HL | DST_Addr_HL_Inc | DST_Addr_HL_Dec =>
            return Bus_Read_Byte (GB, GB.CPU.Regs.HL);

         when DST_Addr_Imm16                                  =>
            return Bus_Read_Byte (GB, Fetch_Imm16 (GB));

         when DST_High_Addr_C                                 =>
            return Bus_Read_Byte (GB, 16#FF00# + Word (GB.CPU.Regs.C));

         when DST_High_Addr_Imm8                              =>
            return Bus_Read_Byte (GB, 16#FF00# + Word (Fetch_Imm8 (GB)));
      end case;
   end Load_Target;

   function Fetch_Imm8 (GB : in out Gade.GB.GB_Type) return Byte is
      Value : constant Byte := Bus_Read_Byte (GB, GB.CPU.PC);
   begin
      GB.CPU.PC := GB.CPU.PC + 1;
      return Value;
   end Fetch_Imm8;

   function Fetch_Imm16 (GB : in out Gade.GB.GB_Type) return Word is
      Lo : constant Byte := Fetch_Imm8 (GB);
      Hi : constant Byte := Fetch_Imm8 (GB);
   begin
      return Word (Lo) + Word (Hi) * 2**8;
   end Fetch_Imm16;

   function Read_Word_Register
     (GB : Gade.GB.GB_Type; Target : Word_Register_Kind) return Word is
   begin
      case Target is
         when REG_AF =>
            return GB.CPU.Regs.AF;

         when REG_BC =>
            return GB.CPU.Regs.BC;

         when REG_DE =>
            return GB.CPU.Regs.DE;

         when REG_HL =>
            return GB.CPU.Regs.HL;

         when REG_SP =>
            return GB.CPU.Regs.SP;
      end case;
   end Read_Word_Register;

   function Read_Word_Source
     (GB : in out Gade.GB.GB_Type; Source : Word_Source_Kind) return Word is
   begin
      case Source is
         when WSRC_BC    =>
            return GB.CPU.Regs.BC;

         when WSRC_DE    =>
            return GB.CPU.Regs.DE;

         when WSRC_HL    =>
            return GB.CPU.Regs.HL;

         when WSRC_SP    =>
            return GB.CPU.Regs.SP;

         when WSRC_Imm16 =>
            return Fetch_Imm16 (GB);
      end case;
   end Read_Word_Source;

   procedure Write_Word_Register
     (GB : in out Gade.GB.GB_Type; Target : Word_Register_Kind; Value : Word) is
   begin
      case Target is
         when REG_AF =>
            GB.CPU.Regs.AF := Value and 16#FFF0#;

         when REG_BC =>
            GB.CPU.Regs.BC := Value;

         when REG_DE =>
            GB.CPU.Regs.DE := Value;

         when REG_HL =>
            GB.CPU.Regs.HL := Value;

         when REG_SP =>
            GB.CPU.Regs.SP := Value;
      end case;
   end Write_Word_Register;

   procedure Push_Word (GB : in out Gade.GB.GB_Type; Value : Word) is
      Hi : constant Byte := Byte (Value / 2**8);
      Lo : constant Byte := Byte (Value and 16#00FF#);
   begin
      GB.CPU.Regs.SP := GB.CPU.Regs.SP - 1;
      Bus_Write_Byte (GB, GB.CPU.Regs.SP, Hi);
      GB.CPU.Regs.SP := GB.CPU.Regs.SP - 1;
      Bus_Write_Byte (GB, GB.CPU.Regs.SP, Lo);
   end Push_Word;

   procedure Pop_Word (GB : in out Gade.GB.GB_Type; Value : out Word) is
      Lo : constant Byte := Bus_Read_Byte (GB, GB.CPU.Regs.SP);
      Hi : Byte;
   begin
      GB.CPU.Regs.SP := GB.CPU.Regs.SP + 1;
      Hi := Bus_Read_Byte (GB, GB.CPU.Regs.SP);
      GB.CPU.Regs.SP := GB.CPU.Regs.SP + 1;
      Value := Word (Lo) + Word (Hi) * 2**8;
   end Pop_Word;

   procedure Adjust_HL_Auto (GB : in out Gade.GB.GB_Type; Source : Byte_Source_Kind) is
   begin
      case Source is
         when SRC_Addr_HL_Inc =>
            GB.CPU.Regs.HL := GB.CPU.Regs.HL + 1;

         when SRC_Addr_HL_Dec =>
            GB.CPU.Regs.HL := GB.CPU.Regs.HL - 1;

         when others          =>
            null;
      end case;
   end Adjust_HL_Auto;

   procedure Adjust_HL_Auto (GB : in out Gade.GB.GB_Type; Target : Byte_Target_Kind) is
   begin
      case Target is
         when DST_Addr_HL_Inc =>
            GB.CPU.Regs.HL := GB.CPU.Regs.HL + 1;

         when DST_Addr_HL_Dec =>
            GB.CPU.Regs.HL := GB.CPU.Regs.HL - 1;

         when others          =>
            null;
      end case;
   end Adjust_HL_Auto;

   procedure ALU_A_Source (GB : in out Gade.GB.GB_Type) is
      Value : constant Byte := Fetch_Source (GB, Source);
      Dummy : Byte;
   begin
      case Operation is
         when ALU_ADD =>
            Gade.Dev.CPU.Instructions.Arithmetic.Add
              (GB.CPU,
               Value,
               GB.CPU.Regs.A,
               Gade.Dev.CPU.Instructions.Arithmetic.ADD_Carry);

         when ALU_ADC =>
            Gade.Dev.CPU.Instructions.Arithmetic.Add
              (GB.CPU,
               Value,
               GB.CPU.Regs.A,
               Gade.Dev.CPU.Instructions.Arithmetic.ADC_Carry);

         when ALU_SUB =>
            Gade.Dev.CPU.Instructions.Arithmetic.Sub
              (GB.CPU,
               Value,
               GB.CPU.Regs.A,
               Gade.Dev.CPU.Instructions.Arithmetic.SUB_Carry);

         when ALU_SBC =>
            Gade.Dev.CPU.Instructions.Arithmetic.Sub
              (GB.CPU,
               Value,
               GB.CPU.Regs.A,
               Gade.Dev.CPU.Instructions.Arithmetic.SBC_Carry);

         when ALU_AND =>
            Gade.Dev.CPU.Instructions.Logic.Logic_AND (GB.CPU, Value);

         when ALU_XOR =>
            Gade.Dev.CPU.Instructions.Logic.Logic_XOR (GB.CPU, Value);

         when ALU_OR  =>
            Gade.Dev.CPU.Instructions.Logic.Logic_OR (GB.CPU, Value);

         when ALU_CP  =>
            Gade.Dev.CPU.Instructions.Arithmetic.Sub
              (GB.CPU, Value, Dummy, Gade.Dev.CPU.Instructions.Arithmetic.SUB_Carry);
      end case;
   end ALU_A_Source;

end Gade.Dev.CPU.Instructions;
