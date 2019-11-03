with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

with Gade.Carts.Memory_Contents;    use Gade.Carts.Memory_Contents;
with Gade.Carts.Plain.Constructors; use Gade.Carts.Plain.Constructors;
with Gade.Carts.MBC1.Constructors;  use Gade.Carts.MBC1.Constructors;
with Gade.Carts.MBC2.Constructors;  use Gade.Carts.MBC2.Constructors;
with Gade.Carts.MBC3.Constructors;  use Gade.Carts.MBC3.Constructors;

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

   function Load_ROM (Path : String) return Cart_NN_Access is
      ROM        : ROM_Content_Access;
      Header     : Cart_Header_Access;
      Controller : Controller_Type;

      Save : constant String := RAM_Path (Path);

      C : Cart_Access;
   begin
      ROM := Load (Path);

      --  Not accurate for some few rare cart types (multicarts)
      Header := Get_Header (ROM);
      Controller := Controller_Type_For_Cart (Header.Cart_Type);

      Put_Line ("Cartridge type: " & Header.Cart_Type'Img);
      Put_Line ("Controller type: " & Controller'Img);

      case Controller is
         when Cartridge_Info.None =>
            C := Cart_Access (Plain.Constructors.Create (ROM, Header, Save));
         when Cartridge_Info.MBC1 =>
            C := Cart_Access (MBC1.Constructors.Create (ROM, Header, Save));
         when Cartridge_Info.MBC2 =>
            C := Cart_Access (MBC2.Constructors.Create (ROM, Header, Save));
         when Cartridge_Info.MBC3 =>
            C := Cart_Access (MBC3.Constructors.Create (ROM, Header, Save));
         when others =>
            C := Cart_Access (Plain.Constructors.Create (ROM, Header, Save));
      end case;

      C.Load_RAM;

      return C;
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
