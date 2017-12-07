with Ada.Text_IO; use Ada.Text_IO;

with Gade.Cartridge.ROM;      use Gade.Cartridge.ROM;
with Gade.Cartridge.MBC.MBC1; use Gade.Cartridge.MBC.MBC1;

package body Gade.Cartridge is

   procedure Load_ROM
     (ROM_Handler : out ROM_Handler_Access;
      RAM_Handler : out RAM_Handler_Access;
      Path        : String)
   is
      pragma Unreferenced (RAM_Handler);
      ROM : constant ROM_Access := new ROM_Type;
      Header : Cartridge_Header_Access;

      function Convert is new Ada.Unchecked_Conversion
        (Source => ROM_Bank_Access,
         Target => Cartridge_Header_Access);

      Controller : Controller_Type;
   begin
      Load (ROM.all, Path);
      Header := Convert (ROM (0)); --  Not true for some few rare cart types
      Controller := Controller_Type_For_Cartridge (Header.Cartridge_Type);

      Put_Line ("Cartridge type: " & Header.Cartridge_Type'Img);
      Put_Line ("Controller type: " & Controller'Img);

      case Controller is
         when Cartridge_Info.None => ROM_Handler := new ROM_Only_Handler_Type;
         when Cartridge_Info.MBC1 => ROM_Handler := new MBC1_ROM_Handler_Type;
         when others => raise Program_Error with "Unsupported cartridge controller! " & Controller'Img;
      end case;

      ROM_Handler.Create (ROM);
      RAM_Handler := new RAM_Handler_Type;
   end Load_ROM;

   procedure Create
     (Handler : out ROM_Handler_Type;
      ROM     : ROM_Access)
   is
   begin
      Handler.ROM := ROM;
      for Bank_Index in Addressable_Bank_Range loop
         Handler.Set_ROM_Bank (Bank_Index, Bank_Index);
      end loop;
   end Create;

   overriding
   procedure Read
     (Handler : in out ROM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
      pragma Unreferenced (GB);
      Bank_Address : constant ROM_Bank_Address := Address and Bank_Address_Mask;
   begin
      case External_ROM_IO_Address (Address) is
         when Bank0_Address => Content := Handler.Banks (0)(Bank_Address);
         when Bank1_Address => Content := Handler.Banks (1)(Bank_Address);
      end case;
   end Read;

   procedure Set_ROM_Bank
     (Handler          : in out ROM_Handler_Type;
      Addressable_Bank : Addressable_Bank_Range;
      ROM_Bank         : ROM_Bank_Range)
   is
   begin
      Handler.Banks (Addressable_Bank) := Handler.ROM (ROM_Bank);
   end Set_ROM_Bank;

   overriding
   procedure Read
     (Handler : in out RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte) is
      pragma Unreferenced (Handler, GB, Address);
   begin
      Content := 16#FF#;
   end Read;

end Gade.Cartridge;
