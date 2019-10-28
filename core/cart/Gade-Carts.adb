with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

--  with Gade.Cart.ROM;                 use Gade.Cart.ROM;
--  with Gade.Cart.Spaces.ROM;          use Gade.Cart.Spaces.ROM;
--  with Gade.Cart.Spaces.ROM.Plain;    use Gade.Cart.Spaces.ROM.Plain;
--  with Gade.Cart.Spaces.ROM.MBC.MBC1; use Gade.Cart.Spaces.ROM.MBC.MBC1;
--  with Gade.Cart.Spaces.ROM.MBC.MBC2; use Gade.Cart.Spaces.ROM.MBC.MBC2;
--  with Gade.Cart.Spaces.ROM.MBC.MBC3; use Gade.Cart.Spaces.ROM.MBC.MBC3;
--
--  with Gade.Cart.Spaces.RAM;             use Gade.Cart.Spaces.RAM;
--  with Gade.Cart.Spaces.RAM.Blank;       use Gade.Cart.Spaces.RAM.Blank;
--  with Gade.Cart.Spaces.RAM.Banked;      use Gade.Cart.Spaces.RAM.Banked;
--  with Gade.Cart.Spaces.RAM.Banked.MBC3; use Gade.Cart.Spaces.RAM.Banked.MBC3;
--  with Gade.Cart.Spaces.RAM.MBC2;        use Gade.Cart.Spaces.RAM.MBC2;

with Gade.Carts.Memory_Contents;    use Gade.Carts.Memory_Contents;
with Gade.Carts.Plain.Constructors; use Gade.Carts.Plain.Constructors;
with Gade.Carts.MBC1.Constructors;  use Gade.Carts.MBC1.Constructors;
with Gade.Carts.MBC2.Constructors;  use Gade.Carts.MBC2.Constructors;

package body Gade.Carts is

   function Get_Header
     (Content : ROM_Content_Access)
      return Cart_Header_Access;

   procedure Read_ROM
     (C       : in out Cart;
      Address : External_ROM_IO_Address;
      V       : out Byte)
   is
      pragma Unreferenced (C, Address);
   begin
      V := Blank_Value;
   end Read_ROM;

   procedure Read_RAM
     (C       : in out Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte)
   is
      pragma Unreferenced (C, Address);
   begin
      V := Blank_Value;
   end Read_RAM;

   function RAM_Path (ROM_Path : String) return String;
   --  function RTC_Path (ROM_Path : String) return String;

--     procedure Load_ROM
--       (ROM_Handler : out Spaces.ROM.Handler_Access;
--        RAM_Handler : out Spaces.RAM.Handler_Access;
--        Path        : String)
--     is
--        ROM_Content : ROM.Content_Access;
--        Header      : Cart_Header_Access;
--
--        --  TODO: FOR DEBUG
--        --  RTC : Gade.Cart.RTC.Clock;
--     begin
--        ROM_Content := Load (Path);
--
--        --  Not true for some few rare cart types (multicarts)
--        Header := Get_Header (ROM_Content);
--
--        RAM_Handler := Create_RAM_Space_Handler (Header.all, Path);
--        ROM_Handler := Create_ROM_Space_Handler (Header.all, ROM_Content, RAM_Handler);
--
--        --  Put_Line ("Reading RTC");
--        --  Gade.Cart.RTC.Load (RTC_Path (Path), RTC);
--        --  delay 2.0;
--        --  Gade.Cart.RTC.Save (RTC_Path (Path) & '2', RTC);
--        --  Put_Line ("Finished Reading RTC");
--     end Load_ROM;
--
   function Load_ROM (Path : String) return Cart_NN_Access is
      ROM        : ROM_Content_Access;
      Header     : Cart_Header_Access;
      Controller : Controller_Type;

      Save : constant String := RAM_Path (Path);
   begin
      ROM := Load (Path);

      --  Not true for some few rare cart types (multicarts)
      Header := Get_Header (ROM);
      Controller := Controller_Type_For_Cart (Header.Cart_Type);

      Put_Line ("Cartridge type: " & Header.Cart_Type'Img);
      Put_Line ("Controller type: " & Controller'Img);

      case Controller is
         when Cartridge_Info.None =>
            return Cart_Access (Plain.Constructors.Create (ROM, Header, Save));
         when Cartridge_Info.MBC1 =>
            return Cart_Access (MBC1.Constructors.Create (ROM, Header, Save));
         when Cartridge_Info.MBC2 =>
            return Cart_Access (MBC2.Constructors.Create (ROM, Header, Save));
         when others =>
            return Cart_Access (Plain.Constructors.Create (ROM, Header, Save));
      end case;
   end Load_ROM;

   function RAM_Path (ROM_Path : String) return String is
   begin
      return
        Compose (Containing_Directory => Containing_Directory (ROM_Path),
                 Name                 => Base_Name (ROM_Path),
                 Extension            => "sav");
   end RAM_Path;

--     function RTC_Path (ROM_Path : String) return String is
--     begin
--        return
--          Compose (Containing_Directory => Containing_Directory (ROM_Path),
--                   Name                 => Base_Name (ROM_Path),
--                   Extension            => "rtc");
--     end RTC_Path;

--     function Get_Header
--       (Content : Cart.ROM.Content_Access)
--        return Cart_Header_Access
--     is
--        type Byte_Access is access all Byte;
--
--        function Convert is new Ada.Unchecked_Conversion
--          (Source => Byte_Access,
--           Target => Cart_Header_Access);
--     begin
--        return Convert (Content (0)'Access);
--     end Get_Header;

   function Get_Header
     (Content : ROM_Content_Access)
      return Cart_Header_Access
   is
      type Byte_Access is access all Byte;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Byte_Access,
         Target => Cart_Header_Access);
   begin
      return Convert (Content (0)'Access);
   end Get_Header;

end Gade.Carts;
