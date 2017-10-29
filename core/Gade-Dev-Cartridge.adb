with Ada.Text_IO;
use Ada.Text_IO;
with Gade.GB; use Gade.GB;
with Gade.Dev.External_RAM; use Gade.Dev.External_RAM;

package body Gade.Dev.Cartridge is
   use Bank_Reader;

   overriding
   procedure Reset (External_ROM : in out External_ROM_Type) is
   begin
      null;
   end Reset;

   procedure Switch_Banks
     (External_ROM : in out External_ROM_Type;
      Bank         : ROM_Banks_Range) is
      Corrected_Bank : ROM_Banks_Range;
   begin
      if Bank = 0 then
         Corrected_Bank := 1;
      else
         Corrected_Bank := Bank;
      end if;
      if External_ROM.Controller = MBC1 and External_ROM.MBC1_Mem_Model = Mode_4_32 then
         --  Limit bank selection to lower 6 bits in RAM Banking Mode
         Corrected_Bank := ROM_Banks_Range (Byte (Corrected_Bank) and Byte (16#1F#));
      end if;
      External_ROM.Current_Bank_1_Idx := Corrected_Bank;
      External_ROM.Current_Bank_1 := External_ROM.Banks (Corrected_Bank);
   end Switch_Banks;

   procedure Load_Bank
     (Bank : out ROM_Bank_Access;
      ROM_File : in Bank_Reader.File_Type) is
   begin
      Bank := new ROM_Bank_Type;
      Read (ROM_File, Bank.Space);
   end Load_Bank;

   --  TODO: Rename, organize
   procedure Print_ROM_Info
     (External_ROM : in out External_ROM_Type) is
      ROM_Bank_0 : constant ROM_Bank_Access := External_ROM.Banks (0);
      Controller : Controller_Type;
      Cartridge_Kind : Cartridge_Type;
   begin
      Cartridge_Kind := ROM_Bank_0.Cartridge_Info.Cartridge_Type;
      Put_Line ("Cartridge type: " & Cartridge_Kind'Img);
      Controller := Controller_Type_For_Cartridge (Cartridge_Kind);
      Put_Line ("Controller type: " & Controller'Img);
      if Controller not in Supported_Controller_Type then
         Put_Line ("Unsupported cartridge controller! " & Controller'Img);
         raise Program_Error;
      end if;
      External_ROM.Controller := Controller;
   end Print_ROM_Info;

   procedure Load_ROM (External_ROM : out External_ROM_Type;
                       Path         : String) is
      ROM_File : Bank_Reader.File_Type;
      Bank_Index : ROM_Banks_Range;
   begin
      External_ROM.Banks := (others => null);
      Open (ROM_File, In_File, Path);
      Bank_Index := 0;
      while not End_Of_File (ROM_File) loop
         Ada.Text_IO.Put_Line ("Loading bank " & Bank_Index'Img);
         Load_Bank (External_ROM.Banks (Bank_Index), ROM_File);
         Bank_Index := Bank_Index + 1;
      end loop;
      Put_Line ("Loader: Successfully loaded" & Bank_Index'Img & " ROM banks.");
      Switch_Banks (External_ROM, 1);
      External_ROM.MBC1_Mem_Model := Mode_16_8;
      Print_ROM_Info (External_ROM);
      Close (ROM_File);
   end Load_ROM;

   subtype RAM_Bank_0_IO_Address is Word range 16#0000# .. 16#3FFF#;
   subtype RAM_Bank_1_IO_Address is Word range 16#4000# .. 16#7FFF#;

   overriding
   procedure Read
     (External_ROM : in out External_ROM_Type;
      GB           : in out Gade.GB.GB_Type;
      Address      : Word;
      Content      : out Byte) is
      pragma Unreferenced (GB);
      Address_Mask : constant Word := 16#3FFF#;
   begin
      if Address in RAM_Bank_0_IO_Address'Range then
         Content := External_ROM.Banks (0).Space (Address and Address_Mask);
      elsif Address in RAM_Bank_1_IO_Address'Range then
         Content := External_ROM.Current_Bank_1.Space (Address and Address_Mask);
      end if;
   end Read;

   procedure Write_MBC1
     (External_ROM : in out External_ROM_Type;
      GB           : in out Gade.GB.GB_Type;
      Address      : External_ROM_IO_Address;
      Content      : Byte) is
      subtype RAM_Bank_Enable_IO_Address is Word range 16#0000# .. 16#1FFF#;
      subtype ROM_Bank_Select_IO_Address is Word range 16#2000# .. 16#3FFF#;
      subtype Mode_Specific_Select_IO_Address is Word range 16#4000# .. 16#5FFF#;
      subtype Mem_Model_Select_IO_Address is Word range 16#6000# .. 16#7FFF#;
   begin
      if Address in RAM_Bank_Enable_IO_Address'Range then
         Set_Enabled (GB.External_RAM, (Content and 16#0F#) = 16#0A#);
      elsif Address in ROM_Bank_Select_IO_Address'Range then
         Switch_Banks (External_ROM, ROM_Banks_Range (Content and 16#1F#));
      elsif
        Address in Mode_Specific_Select_IO_Address'Range and
        External_ROM.MBC1_Mem_Model = Mode_16_8 -- ROM Banking Mode
      then
         Switch_Banks (External_ROM, ROM_Banks_Range ((Content and 16#03#) or
                         (Byte (External_ROM.Current_Bank_1_Idx) and 16#0F#)));
      elsif
        Address in Mode_Specific_Select_IO_Address'Range and
        External_ROM.MBC1_Mem_Model = Mode_4_32 -- RAM Banking Mode
      then
         Switch_Banks (GB.External_RAM, External_RAM_Bank_Range (Content and 16#03#));
      elsif Address in Mem_Model_Select_IO_Address'Range then
         External_ROM.MBC1_Mem_Model := MBC1_Mem_Model_Type'Val (Content and 1);
      end if;
   end Write_MBC1;

   procedure Write_MBC2
     (External_ROM : in out External_ROM_Type;
      GB           : in out Gade.GB.GB_Type;
      Address      : External_ROM_IO_Address;
      Content      : Byte) is
      subtype RAM_Bank_Enable_IO_Address is Word range 16#0000# .. 16#1FFF#;
      subtype ROM_Bank_Select_IO_Address is Word range 16#2000# .. 16#3FFF#;
   begin
      if Address in RAM_Bank_Enable_IO_Address'Range then
         Set_Enabled (GB.External_RAM, (Address and 16#0100#) = 0);
      elsif Address in ROM_Bank_Select_IO_Address'Range then
         Switch_Banks (External_ROM, ROM_Banks_Range (Content and 16#0F#));
      end if;
   end Write_MBC2;

   overriding
   procedure Write
     (External_ROM : in out External_ROM_Type;
      GB           : in out Gade.GB.GB_Type;
      Address      : Word;
      Content      : Byte) is
   begin
      case External_ROM.Controller is
         when MBC1 => Write_MBC1 (External_ROM, GB, Address, Content);
         when MBC2 => Write_MBC2 (External_ROM, GB, Address, Content);
         when others => null;
      end case;
   end Write;

end Gade.Dev.Cartridge;
