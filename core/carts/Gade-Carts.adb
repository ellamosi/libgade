with Ada.Directories;
with Ada.Text_IO;

with Gade.Carts.Mem.ROM;            use Gade.Carts.Mem.ROM;
with Gade.Carts.Plain.Constructors; use Gade.Carts.Plain.Constructors;
with Gade.Carts.MBC1.Constructors;  use Gade.Carts.MBC1.Constructors;
with Gade.Carts.MBC2.Constructors;  use Gade.Carts.MBC2.Constructors;
with Gade.Carts.MBC3.Constructors;  use Gade.Carts.MBC3.Constructors;

package body Gade.Carts is

   package Plain_Carts renames Plain.Constructors;
   package MBC1_Carts renames MBC1.Constructors;
   package MBC2_Carts renames MBC2.Constructors;
   package MBC3_Carts renames MBC3.Constructors;

   function Get_Header
     (Content : ROM_Content_Access)
      return Cart_Header_Access;

   function RAM_Path (ROM_Path : String) return String;

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

   function Load_ROM (Path : String) return Cart_NN_Access is
      use Ada.Text_IO;

      ROM        : ROM_Content_Access;
      Header     : Cart_Header_Access;
      Controller : Controller_Type;

      Save_Path : constant String := RAM_Path (Path);

      C : Cart_Access;
   begin
      ROM := Load (Path);

      Header := Get_Header (ROM);
      Controller := Controller_Type_For_Cart (Header.Cart_Type);

      Put_Line ("Cartridge type: " & Header.Cart_Type'Img);
      Put_Line ("Controller type: " & Controller'Img);

      case Controller is
         when Cartridge_Info.None =>
            C := Cart_Access (Plain_Carts.Create (ROM, Header.all, Save_Path));
         when Cartridge_Info.MBC1 =>
            C := Cart_Access (MBC1_Carts.Create (ROM, Header.all, Save_Path));
         when Cartridge_Info.MBC2 =>
            C := Cart_Access (MBC2_Carts.Create (ROM, Header.all, Save_Path));
         when Cartridge_Info.MBC3 =>
            C := Cart_Access (MBC3_Carts.Create (ROM, Header.all, Save_Path));
         when others =>
            C := Cart_Access (Plain_Carts.Create (ROM, Header.all, Save_Path));
      end case;

      C.Load_RAM;

      return C;
   end Load_ROM;

   procedure Load_RAM (C : in out Cart) is
      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      if C.Persistent then
         Open (File, In_File, C.Save_Path.all);
         Cart'Class (C).Load_RAM_File (File);
         Close (File);
      end if;
   exception
      when Name_Error =>
         null; --  TODO: Log "save not found" when having a propper logger
   end Load_RAM;

   procedure Save_RAM (C : in out Cart) is
      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      if C.Persistent then
         Create (File, Out_File, C.Save_Path.all);
         Cart'Class (C).Save_RAM_File (File);
         Close (File);
      end if;
   end Save_RAM;

   function RAM_Path (ROM_Path : String) return String is
      use Ada.Directories;
   begin
      return
        Compose (Containing_Directory => Containing_Directory (ROM_Path),
                 Name                 => Base_Name (ROM_Path),
                 Extension            => "sav");
   end RAM_Path;

   function Get_Header
     (Content : ROM_Content_Access)
      return Cart_Header_Access
   is
      type Byte_Access is access all Byte;

      pragma Warnings
        (Off, "possible aliasing problem for type ""Cart_Header_Access""");
      --  Allow strict aliasing analysis optimizations as type conversion is for
      --  a read only use.
      function Convert is new Ada.Unchecked_Conversion
        (Source => Byte_Access,
         Target => Cart_Header_Access);
      pragma Warnings
        (On, "possible aliasing problem for type ""Cart_Header_Access""");
   begin
      return Convert (Content (0)'Access);
   end Get_Header;

end Gade.Carts;
