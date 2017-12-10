with Ada.Text_IO;

package body Gade.External.ROM is

   procedure Load
     (ROM  : out ROM_Type;
      Path : String)
   is
      File       : Bank_Reader.File_Type;
      Bank_Index : ROM_Bank_Range;
   begin
      ROM := (others => Null_ROM_Bank'Access);
      Open (File, In_File, Path);
      Bank_Index := 0;
      while not End_Of_File (File) loop
         Ada.Text_IO.Put_Line ("Loading bank " & Bank_Index'Img);
         Load_Bank (File, ROM (Bank_Index));
         Bank_Index := Bank_Index + 1;
      end loop;
      Ada.Text_IO.Put_Line ("Loader: Successfully loaded" & Bank_Index'Img & " ROM banks.");
      Close (File);
   end Load;

   procedure Load_Bank
     (File : File_Type;
      Bank : out ROM_Bank_Access)
   is
      Target_Bank : constant access ROM_Bank_Type := new ROM_Bank_Type;
   begin
      Read (File, Target_Bank.all);
      Bank := ROM_Bank_Access (Target_Bank);
   end Load_Bank;

   function Create
     (ROM  : ROM_Type;
      Size : ROM_Size_Type) return Switchable_Bank_ROM_Access
   is
      Result : constant Switchable_Bank_ROM_Access := new Switchable_Bank_ROM_Type;
      Bank_Count : constant ROM_Bank_Count := Bank_Count_For_Size (Size);
   begin
      Result.Banks := new ROM_Type (0 .. Bank_Count - 1);
      Result.Banks.all := ROM (Result.Banks'Range);
      for Addressable_Bank in Result.Addressable_Banks'Range loop
         Result.Addressable_Banks (Addressable_Bank) :=
           Result.Banks (Addressable_Bank);
      end loop;
      return Result;
   end Create;

   procedure Read
     (ROM     : Switchable_Bank_ROM_Type;
      Address : External_ROM_IO_Address;
      Content : out Byte)
   is
      Bank_Address : constant ROM_Bank_Address := Address and Bank_Address_Mask;
   begin
      case Address is
         when Bank0_Address => Content := ROM.Addressable_Banks (0)(Bank_Address);
         when Bank1_Address => Content := ROM.Addressable_Banks (1)(Bank_Address);
      end case;
   end Read;

   procedure Switch_Banks
     (ROM              : in out Switchable_Bank_ROM_Type;
      Addressable_Bank : Addressable_Bank_Range;
      ROM_Bank         : ROM_Bank_Range)
   is
      --  MBC1:
      --  If the cartridge ROM is smaller than 16 Mbit, there are less ROM
      --  address pins to connect to and therefore some bank number bits are
      --  ignored. For example, 4 Mbit ROMs only need a 5-bit bank number, so
      --  the BANK2 register value is always ignored because those bits are
      --  simply not connected to the ROM.
      Bank : constant ROM_Bank_Range := ROM_Bank mod ROM.Banks'Length;
   begin
      ROM.Addressable_Banks (Addressable_Bank) := ROM.Banks (Bank);
   end Switch_Banks;

end Gade.External.ROM;
