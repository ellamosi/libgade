with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;  use Ada.Directories;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with Gade.Audio_Buffer; use Gade.Audio_Buffer;
with Gade.Camera;
with Gade.Interfaces;   use Gade.Interfaces;
with Gade.Video_Buffer; use Gade.Video_Buffer;

procedure Gade.Camera_Run_For_Check is

   use Ada.Streams.Stream_IO;

   ROM_Bank_Size : constant := 16#4000#;
   ROM_Banks     : constant := 64;

   Bank_Size : constant := 16#2000#;

   ROM_Path  : constant String := "/tmp/gade-camera-run-for-check.gb";
   Save_Path : constant String := "/tmp/gade-camera-run-for-check.sav";

   Trigger_Program : constant array (Natural range 0 .. 57) of Byte :=
     [16#3E#,
      16#0A#,
      16#EA#,
      16#00#,
      16#00#,
      16#3E#,
      16#10#,
      16#EA#,
      16#00#,
      16#40#,
      16#AF#,
      16#EA#,
      16#02#,
      16#A0#,
      16#3E#,
      16#30#,
      16#EA#,
      16#03#,
      16#A0#,
      16#3E#,
      16#07#,
      16#EA#,
      16#00#,
      16#A0#,
      16#FA#,
      16#00#,
      16#A0#,
      16#E6#,
      16#01#,
      16#20#,
      16#F9#,
      16#AF#,
      16#EA#,
      16#00#,
      16#40#,
      16#FA#,
      16#00#,
      16#A1#,
      16#47#,
      16#FA#,
      16#01#,
      16#A1#,
      16#4F#,
      16#3E#,
      16#01#,
      16#EA#,
      16#00#,
      16#40#,
      16#78#,
      16#EA#,
      16#00#,
      16#A0#,
      16#79#,
      16#EA#,
      16#01#,
      16#A0#,
      16#18#,
      16#FE#];

   type Solid_Camera_Provider is limited new Gade.Camera.Provider_Interface
   with null record;

   function Byte_Image (Value : Byte) return String;

   procedure Assert (Condition : Boolean; Message : String);

   procedure Cleanup;

   procedure Create_Test_ROM;

   procedure Expect_Save_Byte
     (Offset : Natural; Expected : Byte; Label : String);

   procedure Run_Frame
     (G     : Gade_Type;
      Video : RGB32_Display_Buffer_Access;
      Audio : Audio_Buffer_Access);

   overriding
   procedure Capture_Frame
     (Provider : Solid_Camera_Provider; Frame : out Gade.Camera.Bitmap);

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

   overriding
   procedure Capture_Frame
     (Provider : Solid_Camera_Provider; Frame : out Gade.Camera.Bitmap)
   is
      pragma Unreferenced (Provider);
   begin
      for Y in Frame'Range (1) loop
         for X in Frame'Range (2) loop
            Frame (Y, X) := 2;
         end loop;
      end loop;
   end Capture_Frame;

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
            Value := 16#00#;

            if Bank = 0 then
               case Offset is
                  when 16#0100# .. 16#0100# + Trigger_Program'Last =>
                     Value := Trigger_Program (Offset - 16#0100#);

                  when 16#0147#                                    =>
                     Value := 16#FC#;

                  when 16#0148#                                    =>
                     Value := 16#05#;

                  when 16#0149#                                    =>
                     Value := 16#04#;

                  when others                                      =>
                     null;
               end case;
            end if;

            Byte'Write (Stream, Value);
         end loop;
      end loop;

      Close (File);
   end Create_Test_ROM;

   procedure Expect_Save_Byte
     (Offset : Natural; Expected : Byte; Label : String)
   is
      File   : File_Type;
      Stream : Stream_Access;
      Actual : Byte;
   begin
      Open (File, In_File, Save_Path);
      Set_Index (File, Positive_Count (Offset + 1));
      Stream := Ada.Streams.Stream_IO.Stream (File);
      Byte'Read (Stream, Actual);
      Close (File);

      Assert
        (Actual = Expected,
         Label
         & ": expected"
         & Byte_Image (Expected)
         & " got"
         & Byte_Image (Actual));
   end Expect_Save_Byte;

   procedure Run_Frame
     (G     : Gade_Type;
      Video : RGB32_Display_Buffer_Access;
      Audio : Audio_Buffer_Access)
   is
      Requested_Samples : constant Positive := 2_048;
      Generated_Samples : Natural;
      Frame_Finished    : Boolean := False;
   begin
      while not Frame_Finished loop
         Run_For
           (G,
            Requested_Samples,
            Generated_Samples,
            Video,
            Audio,
            Frame_Finished);
      end loop;
   end Run_Frame;

begin
   Create_Test_ROM;

   declare
      Audio    : aliased Frame_Audio_Buffer;
      Created  : Boolean := False;
      G        : Gade_Type;
      Provider : aliased Solid_Camera_Provider;
      Video    : aliased RGB32_Display_Buffer;
   begin
      Gade.Interfaces.Create (G);
      Set_Camera_Provider (G, Provider'Unchecked_Access);
      Created := True;
      Load_ROM (G, ROM_Path);
      Reset (G);

      for I in 1 .. 4 loop
         Run_Frame (G, Video'Unchecked_Access, Audio'Unchecked_Access);
      end loop;

      Finalize (G);
      Created := False;

      Assert (Exists (Save_Path), "expected save file to be created");

      Expect_Save_Byte (16#0100#, 16#00#, "captured low plane in bank 0");
      Expect_Save_Byte (16#0101#, 16#FF#, "captured high plane in bank 0");
      Expect_Save_Byte (Bank_Size + 16#0000#, 16#00#, "CPU copied low plane");
      Expect_Save_Byte (Bank_Size + 16#0001#, 16#FF#, "CPU copied high plane");
   exception
      when others =>
         if Created then
            begin
               Finalize (G);
            exception
               when others =>
                  null;
            end;
         end if;
         raise;
   end;

   Cleanup;
   Ada.Text_IO.Put_Line ("camera run_for check passed");
exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Exception_Message (E));
      Cleanup;
      Set_Exit_Status (Failure);
end Gade.Camera_Run_For_Check;
