with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;  use Ada.Directories;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with Gade.Carts;

procedure Gade.Camera_Controller_Check is

   use Ada.Streams.Stream_IO;

   ROM_Bank_Size : constant := 16#4000#;
   ROM_Banks     : constant := 64;

   ROM_Path  : constant String := "/tmp/gade-camera-controller-check.gb";
   Save_Path : constant String := "/tmp/gade-camera-controller-check.sav";

   function Byte_Image (Value : Byte) return String;

   procedure Assert (Condition : Boolean; Message : String);

   procedure Cleanup;

   procedure Create_Test_ROM;

   procedure Expect_RAM
     (C        : in out Gade.Carts.Cart'Class;
      Address  : Gade.Carts.External_RAM_IO_Address;
      Expected : Byte;
      Label    : String);

   procedure Expect_ROM
     (C        : in out Gade.Carts.Cart'Class;
      Address  : Gade.Carts.External_ROM_IO_Address;
      Expected : Byte;
      Label    : String);

   function Byte_Image (Value : Byte) return String is
   begin
      return Natural'Image (Natural (Value));
   end Byte_Image;

   procedure Assert (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         raise Program_Error with Message;
      end if;
   end Assert;

   procedure Cleanup is
   begin
      if Exists (ROM_Path) then
         Delete_File (ROM_Path);
      end if;
      if Exists (Save_Path) then
         Delete_File (Save_Path);
      end if;
   end Cleanup;

   procedure Create_Test_ROM is
      File   : File_Type;
      Stream : Stream_Access;
      Value  : Byte;
   begin
      Cleanup;
      Create (File, Out_File, ROM_Path);
      Stream := Ada.Streams.Stream_IO.Stream (File);

      for Bank in 0 .. ROM_Banks - 1 loop
         for Offset in 0 .. ROM_Bank_Size - 1 loop
            Value := Byte (Bank);
            if Bank = 0 then
               case Offset is
                  when 16#147# =>
                     Value := 16#FC#;

                  when 16#148# =>
                     Value := 16#05#;

                  when 16#149# =>
                     Value := 16#04#;

                  when others  =>
                     null;
               end case;
            end if;
            Byte'Write (Stream, Value);
         end loop;
      end loop;

      Close (File);
   end Create_Test_ROM;

   procedure Expect_RAM
     (C        : in out Gade.Carts.Cart'Class;
      Address  : Gade.Carts.External_RAM_IO_Address;
      Expected : Byte;
      Label    : String)
   is
      Actual : Byte;
   begin
      Gade.Carts.Read_RAM (C, Address, Actual);
      Assert
        (Actual = Expected,
         Label
         & ": expected"
         & Byte_Image (Expected)
         & " got"
         & Byte_Image (Actual));
   end Expect_RAM;

   procedure Expect_ROM
     (C        : in out Gade.Carts.Cart'Class;
      Address  : Gade.Carts.External_ROM_IO_Address;
      Expected : Byte;
      Label    : String)
   is
      Actual : Byte;
   begin
      Gade.Carts.Read_ROM (C, Address, Actual);
      Assert
        (Actual = Expected,
         Label
         & ": expected"
         & Byte_Image (Expected)
         & " got"
         & Byte_Image (Actual));
   end Expect_ROM;

begin
   Create_Test_ROM;

   declare
      Cart : constant Gade.Carts.Cart_NN_Access :=
        Gade.Carts.Load_ROM (ROM_Path, null);
   begin
      Expect_ROM (Cart.all, 16#4000#, 16#01#, "initial switchable ROM bank");

      Gade.Carts.Write_ROM (Cart.all, 16#2000#, 16#00#);
      Expect_ROM (Cart.all, 16#4000#, 16#00#, "ROM bank 0 is selectable");

      Gade.Carts.Write_ROM (Cart.all, 16#2000#, 16#3F#);
      Expect_ROM
        (Cart.all, 16#4000#, 16#3F#, "highest camera ROM bank is selectable");

      Expect_RAM
        (Cart.all, 16#A123#, 16#FF#, "RAM is readable while write-disabled");
      Gade.Carts.Write_RAM (Cart.all, 16#A123#, 16#12#);
      Expect_RAM
        (Cart.all, 16#A123#, 16#FF#, "RAM writes are ignored while disabled");

      Gade.Carts.Write_ROM (Cart.all, 16#0000#, 16#0A#);
      Gade.Carts.Write_ROM (Cart.all, 16#4000#, 16#02#);
      Gade.Carts.Write_RAM (Cart.all, 16#A123#, 16#55#);
      Expect_RAM (Cart.all, 16#A123#, 16#55#, "RAM bank write while enabled");

      Gade.Carts.Write_ROM (Cart.all, 16#0000#, 16#00#);
      Gade.Carts.Write_RAM (Cart.all, 16#A123#, 16#AA#);
      Expect_RAM (Cart.all, 16#A123#, 16#55#, "RAM writes stop when disabled");

      Gade.Carts.Write_ROM (Cart.all, 16#4000#, 16#10#);
      Gade.Carts.Write_RAM (Cart.all, 16#A001#, 16#77#);
      Expect_RAM
        (Cart.all, 16#A001#, 16#00#, "camera registers are write-only");

      Gade.Carts.Write_RAM (Cart.all, 16#A000#, 16#07#);
      Expect_RAM (Cart.all, 16#A000#, 16#07#, "capture status reads active");
      Expect_RAM
        (Cart.all, 16#A080#, 16#07#, "register mirrors expose capture status");

      Gade.Carts.Write_ROM (Cart.all, 16#4000#, 16#02#);
      Expect_RAM
        (Cart.all, 16#A123#, 16#00#, "active capture blanks RAM reads");
      Gade.Carts.Write_RAM (Cart.all, 16#A123#, 16#99#);
      Expect_RAM
        (Cart.all, 16#A123#, 16#00#, "active capture ignores RAM writes");

      Gade.Carts.Write_ROM (Cart.all, 16#4000#, 16#10#);
      Gade.Carts.Write_RAM (Cart.all, 16#A000#, 16#06#);
      Expect_RAM (Cart.all, 16#A000#, 16#06#, "capture can be paused");

      Gade.Carts.Write_ROM (Cart.all, 16#4000#, 16#02#);
      Expect_RAM
        (Cart.all, 16#A123#, 16#55#, "paused capture restores RAM reads");

      Gade.Carts.Write_ROM (Cart.all, 16#4000#, 16#10#);
      Gade.Carts.Write_RAM (Cart.all, 16#A000#, 16#07#);
      Expect_RAM (Cart.all, 16#A000#, 16#07#, "capture resumes");

      Gade.Carts.Report_Cycles (Cart.all, 40_000);

      Gade.Carts.Write_ROM (Cart.all, 16#4000#, 16#10#);
      Expect_RAM
        (Cart.all, 16#A000#, 16#06#, "capture completes and clears busy bit");

      Gade.Carts.Write_ROM (Cart.all, 16#4000#, 16#00#);
      Expect_RAM (Cart.all, 16#A100#, 16#FF#, "pattern row 0 low plane");
      Expect_RAM (Cart.all, 16#A101#, 16#FF#, "pattern row 0 high plane");
      Expect_RAM (Cart.all, 16#A210#, 16#00#, "pattern shade 0 low plane");
      Expect_RAM (Cart.all, 16#A211#, 16#00#, "pattern shade 0 high plane");
      Expect_RAM (Cart.all, 16#A250#, 16#FF#, "pattern shade 1 low plane");
      Expect_RAM (Cart.all, 16#A251#, 16#00#, "pattern shade 1 high plane");
      Expect_RAM (Cart.all, 16#A290#, 16#00#, "pattern shade 2 low plane");
      Expect_RAM (Cart.all, 16#A291#, 16#FF#, "pattern shade 2 high plane");
      Expect_RAM (Cart.all, 16#A2D0#, 16#FF#, "pattern shade 3 low plane");
      Expect_RAM (Cart.all, 16#A2D1#, 16#FF#, "pattern shade 3 high plane");
   end;

   Cleanup;
   Ada.Text_IO.Put_Line ("camera controller check passed");
exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Exception_Message (E));
      Cleanup;
      Set_Exit_Status (Failure);
end Gade.Camera_Controller_Check;
