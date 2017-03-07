package body Gade.GB.Memory_Map is

   -- 0000: 16kB ROM bank #0 | 32kB Cartridge (First Half)
   -- 4000: 16kB switchable ROM bank | 32kB Cartridge (Second Half)
   -- 8000: 8kB Video RAM
   -- A000: 8kB switchable RAM bank
   -- C000: 8kB Internal RAM
   -- E000: Echo of 8kB Internal RAM
   -- FE00: Sprite Attrib Memory (OAM)
   -- FEA0: Empty but unusable for I/O
   -- FF00: I/O ports
   -- FF4C: Empty but unusable for I/O
   -- FF80: Internal RAM
   -- FFFF: Interrupt Enable Register

   -- TODO: Have a proper internal RAM package
   subtype Internal_RAM_Echo_IO_Address is Word range 16#E000#..16#FDFF#;

   procedure Read_Byte
     (GB      : in out GB_Type;
      Address : Word;
      Value   : out Byte) is
   begin
      case Address is
         when External_ROM_IO_Address =>
            GB.External_ROM.Read(GB, Address, Value);
         when VRAM_IO_Address =>
            GB.Video_RAM.Read(GB, Address, Value);
         when Internal_RAM_Echo_IO_Address =>
            Value := GB.Content(Address - 16#2000#);
         when External_RAM_IO_Address =>
            GB.External_RAM.Read(GB, Address, Value);
         when OAM_IO_Address =>
            GB.Video_OAM.Read(GB, Address, Value);
         when Joypad_IO_Address =>
            GB.Joypad.Read(GB, Address, Value);
         when Timer_IO_Address =>
            GB.Timer.Read(GB, Address, Value);
         when Interrupt_Flag_IO_Address =>
            GB.Interrupt_Flag.Read(GB, Address, Value);
         when Display_IO_Address =>
            GB.Display.Read(GB, Address, Value);
         when Interrupt_Enable_IO_Address =>
            GB.Interrupt_Enable.Read(GB, Address, Value);
         when others =>
            Value := GB.Content(Address);
      end case;
   end Read_Byte;

   function Read_Byte
     (GB      : in out GB_Type;
      Address : Word) return Byte is
      Result : Byte;
   begin
      Read_Byte(GB, Address, Result);
      return Result;
   end Read_Byte;

   procedure Write_Byte
     (GB      : in out GB_Type;
      Address : Word;
      Value   : Byte) is
   begin
      case Address is
         when External_ROM_IO_Address =>
            GB.External_ROM.Write(GB, Address, Value);
         when VRAM_IO_Address =>
            GB.Video_RAM.Write(GB, Address, Value);
         when Internal_RAM_Echo_IO_Address =>
            GB.Content(Address - 16#2000#) := Value;
         when External_RAM_IO_Address =>
            GB.External_RAM.Write(GB, Address, Value);
         when OAM_IO_Address =>
            GB.Video_OAM.Write(GB, Address, Value);
         when Joypad_IO_Address =>
            GB.Joypad.Write(GB, Address, Value);
         when Interrupt_Flag_IO_Address =>
            GB.Interrupt_Flag.Write(GB, Address, Value);
         when Timer_IO_Address =>
            GB.Timer.Write(GB, Address, Value);
         when Display_IO_Address =>
            GB.Display.Write(GB, Address, Value);
         when Interrupt_Enable_IO_Address =>
            GB.Interrupt_Enable.Write(GB, Address, Value);
         when others =>
            GB.Content(Address) := Value;
      end case;
   end Write_Byte;

   procedure Read_Word
     (GB      : in out GB_Type;
      Address : Word;
      Value   : out Word) is
      Byte_Value : Byte;
   begin
      Read_Byte(GB, Address, Byte_Value);
      Value := Word(Byte_Value);
      Read_Byte(GB, Address+1, Byte_Value);
      Value := Value or Word(Byte_Value) * 2**8;
   end Read_Word;

   function Read_Word
     (GB      : in out GB_Type;
      Address : Word) return Word is
      Result : Word;
   begin
      Read_Word(GB, Address, Result);
      return Result;
   end Read_Word;

   procedure Write_Word
     (GB      : in out GB_Type;
      Address : Word;
      Value   : Word) is
   begin
      Write_Byte(GB, Address, Byte(Value mod 2**8));
      Write_Byte(GB, Address+1, Byte(Value / 2**8));
   end Write_Word;

   procedure Push (GB : in out GB_Type; Value : Word) is
   begin
      GB.CPU.Regs.SP := GB.CPU.Regs.SP - 2;
      Write_Word(GB, GB.CPU.Regs.SP, Value);
   end Push;

   procedure Pop (GB : in out GB_Type; Value : out Word) is
   begin
      Read_Word(GB, GB.CPU.Regs.SP, Value);
      GB.CPU.Regs.SP := GB.CPU.Regs.SP + 2;
   end Pop;

end Gade.GB.Memory_Map;
