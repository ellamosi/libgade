with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

with Gade.Cart.ROM;                 use Gade.Cart.ROM;
with Gade.Cart.Spaces.ROM;          use Gade.Cart.Spaces.ROM;
with Gade.Cart.Spaces.ROM.Plain;    use Gade.Cart.Spaces.ROM.Plain;
with Gade.Cart.Spaces.ROM.MBC.MBC1; use Gade.Cart.Spaces.ROM.MBC.MBC1;
with Gade.Cart.Spaces.ROM.MBC.MBC2; use Gade.Cart.Spaces.ROM.MBC.MBC2;

with Gade.Cart.Spaces.RAM;          use Gade.Cart.Spaces.RAM;
with Gade.Cart.Spaces.RAM.Blank;    use Gade.Cart.Spaces.RAM.Blank;
with Gade.Cart.Spaces.RAM.Banked;   use Gade.Cart.Spaces.RAM.Banked;
with Gade.Cart.Spaces.RAM.MBC2;     use Gade.Cart.Spaces.RAM.MBC2;

package body Gade.Cart is

   function Create_RAM_Space_Handler
     (Header   : Cart_Header;
      ROM_Path : String) return Spaces.RAM.Handler_Access;

   function Create_ROM_Space_Handler
     (Header      : Cart_Header;
      ROM_Content : ROM.Content_Access;
      RAM_Handler : Spaces.RAM.Handler_Access) return Spaces.ROM.Handler_Access;

   function Get_Header
     (Content : Cart.ROM.Content_Access)
      return Cart_Header_Access;

   function RAM_Path (ROM_Path : String) return String;

   procedure Load_ROM
     (ROM_Handler : out Spaces.ROM.Handler_Access;
      RAM_Handler : out Spaces.RAM.Handler_Access;
      Path        : String)
   is
      ROM_Content : ROM.Content_Access;
      Header      : Cart_Header_Access;
   begin
      ROM_Content := Load (Path);

      --  Not true for some few rare cart types (multicarts)
      Header := Get_Header (ROM_Content);

      RAM_Handler := Create_RAM_Space_Handler (Header.all, Path);
      ROM_Handler := Create_ROM_Space_Handler (Header.all, ROM_Content, RAM_Handler);
   end Load_ROM;

   function Create_RAM_Space_Handler
     (Header   : Cart_Header;
      ROM_Path : String) return Spaces.RAM.Handler_Access
   is
      package Blank_RAM_Handler  renames Gade.Cart.Spaces.RAM.Blank;
      package Banked_RAM_Handler renames Gade.Cart.Spaces.RAM.Banked;
      package MBC2_RAM_Handler   renames Gade.Cart.Spaces.RAM.MBC2;
      subtype Handler_Access is Spaces.RAM.Handler_Access;
      RAM_Handler_Kind : constant RAM_Handler_Kind_Type :=
        RAM_Handler_Kind_For_Cart (Header.Cart_Type);
      Path : constant String := RAM_Path (ROM_Path);
   begin
      return
        (case RAM_Handler_Kind is
            when None =>
               Handler_Access (Blank_RAM_Handler.Create),
            when MBC1 =>
               Handler_Access (Banked_RAM_Handler.Create (Header.RAM_Size, Path)),
            when MBC2 =>
               Handler_Access (MBC2_RAM_Handler.Create (Path))
        );
   end Create_RAM_Space_Handler;

   function Create_ROM_Space_Handler
     (Header      : Cart_Header;
      ROM_Content : ROM.Content_Access;
      RAM_Handler : Spaces.RAM.Handler_Access) return Spaces.ROM.Handler_Access
   is
      subtype Handler_Access is Spaces.ROM.Handler_Access;
      Controller : constant Controller_Type :=
        Controller_Type_For_Cart (Header.Cart_Type);
   begin
      Put_Line ("Cartridge type: " & Header.Cart_Type'Img);
      Put_Line ("Controller type: " & Controller'Img);
      return
        (case Controller is
            when Cartridge_Info.None =>
               Handler_Access (Spaces.ROM.Plain.Create (ROM_Content)),
            when Cartridge_Info.MBC1 =>
               Handler_Access (Spaces.ROM.MBC.MBC1.Create (ROM_Content, RAM_Handler)),
            when Cartridge_Info.MBC2 =>
               Handler_Access (Spaces.ROM.MBC.MBC2.Create (ROM_Content, RAM_Handler)),
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

   function Get_Header
     (Content : Cart.ROM.Content_Access)
      return Cart_Header_Access
   is
      type Byte_Access is access all Byte;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Byte_Access,
         Target => Cart_Header_Access);
   begin
      return Convert (Content (0)'Access);
   end Get_Header;

end Gade.Cart;
