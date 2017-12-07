with Ada.Text_IO; use Ada.Text_IO;

with Gade.ROM_Handler.ROM;      use Gade.ROM_Handler.ROM;
with Gade.ROM_Handler.MBC.MBC1; use Gade.ROM_Handler.MBC.MBC1;

with Gade.RAM_Handler.Blank;    use Gade.RAM_Handler.Blank;
with Gade.RAM_Handler.MBC1;     use Gade.RAM_Handler.MBC1;

package body Gade.Cartridge is

   procedure Load_ROM
     (ROM_Handler : out ROM_Handler_Access;
      RAM_Handler : out RAM_Handler_Access;
      Path        : String)
   is
      ROM : constant ROM_Access := new ROM_Type;
      Header : Cartridge_Header_Access;

      function Convert is new Ada.Unchecked_Conversion
        (Source => ROM_Bank_Access,
         Target => Cartridge_Header_Access);

      Controller : Controller_Type;
      RAM_Handler_Kind : RAM_Handler_Kind_Type;
   begin
      Load (ROM.all, Path);
      Header := Convert (ROM (0)); --  Not true for some few rare cart types
      Controller := Controller_Type_For_Cartridge (Header.Cartridge_Type);
      RAM_Handler_Kind := RAM_Handler_Kind_For_Cartridge (Header.Cartridge_Type);

      Put_Line ("Cartridge type: " & Header.Cartridge_Type'Img);
      Put_Line ("Controller type: " & Controller'Img);

      case Controller is
         when Cartridge_Info.None => ROM_Handler := new ROM_Only_Handler_Type;
         when Cartridge_Info.MBC1 => ROM_Handler := new MBC1_ROM_Handler_Type;
         when others =>
            raise Program_Error with "Unsupported cartridge controller! " & Controller'Img;
      end case;

      case RAM_Handler_Kind is
         when None => RAM_Handler := new Blank_RAM_Handler_Type;
         when MBC1 => RAM_Handler := new MBC1_RAM_Handler_Type;
      end case;

      RAM_Handler.Create;
      ROM_Handler.Create (ROM, RAM_Handler);
   end Load_ROM;

end Gade.Cartridge;
