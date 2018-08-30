with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

with Gade.Cart.ROM;                use Gade.Cart.ROM;
with Gade.Cart.ROM_Space;          use Gade.Cart.ROM_Space;
with Gade.Cart.ROM_Space.ROM_Only; use Gade.Cart.ROM_Space.ROM_Only;
with Gade.Cart.ROM_Space.MBC.MBC1; use Gade.Cart.ROM_Space.MBC.MBC1;

with Gade.Cart.RAM_Space;          use Gade.Cart.RAM_Space;
with Gade.Cart.RAM_Space.Blank;    use Gade.Cart.RAM_Space.Blank;
with Gade.Cart.RAM_Space.Banked;   use Gade.Cart.RAM_Space.Banked;

package body Gade.Cart is

   function Create_RAM_Space_Handler
     (Header   : Cart_Header;
      ROM_Path : String) return RAM_Space_Access;

   function Create_ROM_Space_Handler
     (Header      : Cart_Header;
      ROM_Content : ROM_Content_Access;
      RAM_Handler : RAM_Space_Access) return ROM_Space_Access;

   function RAM_Path (ROM_Path : String) return String;

   procedure Load_ROM
     (ROM_Handler : out ROM_Space_Access;
      RAM_Handler : out RAM_Space_Access;
      Path        : String)
   is
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access;
   begin
      ROM_Content := Load (Path);

      --  Not true for some few rare cart types (multicarts)
      Header := Gade.Cart.ROM.Header (ROM_Content);

      RAM_Handler := Create_RAM_Space_Handler (Header.all, Path);
      ROM_Handler := Create_ROM_Space_Handler (Header.all, ROM_Content, RAM_Handler);
   end Load_ROM;

   function Create_RAM_Space_Handler
     (Header   : Cart_Header;
      ROM_Path : String) return RAM_Space_Access
   is
      package Blank_RAM_Handler  renames Gade.Cart.RAM_Space.Blank;
      package Banked_RAM_Handler renames Gade.Cart.RAM_Space.Banked;
      RAM_Handler_Kind : constant RAM_Handler_Kind_Type :=
        RAM_Handler_Kind_For_Cart (Header.Cart_Type);
      Path : constant String := RAM_Path (ROM_Path);
   begin
      return
        (case RAM_Handler_Kind is
            when None =>
               RAM_Space_Access (Blank_RAM_Handler.Create),
            when MBC1 =>
               RAM_Space_Access (Banked_RAM_Handler.Create (Header.RAM_Size, Path)));
   end Create_RAM_Space_Handler;

   function Create_ROM_Space_Handler
     (Header      : Cart_Header;
      ROM_Content : ROM_Content_Access;
      RAM_Handler : RAM_Space_Access) return ROM_Space_Access
   is
      package ROM_Only_Handler renames Gade.Cart.ROM_Space.ROM_Only;
      package MBC1_ROM_Handler renames Gade.Cart.ROM_Space.MBC.MBC1;
      Controller : constant Controller_Type :=
        Controller_Type_For_Cart (Header.Cart_Type);
   begin
      Put_Line ("Cartridge type: " & Header.Cart_Type'Img);
      Put_Line ("Controller type: " & Controller'Img);
      return
        (case Controller is
            when Cartridge_Info.None =>
               ROM_Space_Access (ROM_Only_Handler.Create (ROM_Content)),
            when Cartridge_Info.MBC1 =>
               ROM_Space_Access (MBC1_ROM_Handler.Create (ROM_Content, RAM_Handler)),
            when others =>
               raise Program_Error with "Unsupported cartridge controller! " & Controller'Img);
   end Create_ROM_Space_Handler;

   function RAM_Path (ROM_Path : String) return String is
   begin
      return
        Compose (Containing_Directory => Containing_Directory (ROM_Path),
                 Name                 => Base_Name (ROM_Path),
                 Extension            => "sav");
   end RAM_Path;

end Gade.Cart;
