with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Interfaces;
with Image_IO;
with Image_IO.Holders;
with Image_IO.Operations;

with Harness_Input;
with Gade.Interfaces;
with Gade.Video_Buffer;
with Gade.Audio_Buffer;

procedure Gade_Testd is

   use Ada.Characters.Handling;
   use Ada.Exceptions;
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Text_IO;
   use Interfaces;
   use type Image_IO.Color_Info;
   use Gade.Interfaces;
   use Gade.Video_Buffer;
   use Gade.Audio_Buffer;
   Dev_Null : File_Type;

   G : Gade_Type;
   G_Created : Boolean := False;
   ROM_Loaded : Boolean := False;

   V_Buff : aliased RGB32_Display_Buffer;
   A_Buff : aliased Audio_Buffer_Type;

   Input_Reader : aliased Harness_Input.Manual_Input_Reader;

   Frame_Count : Natural := 0;

   function To_Image (Buffer : RGB32_Display_Buffer) return Image_IO.Image_Data;
   function Image_Equal
     (Left  : Image_IO.Image_Data;
      Right : Image_IO.Image_Data) return Boolean;

   procedure Reply_OK (Payload : String := "");
   procedure Reply_ERR (Code : String; Message : String);

   procedure Ensure_Engine;
   procedure Run_Frame;

   function Next_Token (Line : String; Pos : in out Positive) return String;
   function Parse_Natural (S : String; Value : out Natural) return Boolean;
   function Parse_Hex_U8 (S : String; Value : out Unsigned_8) return Boolean;

   function To_Image (Buffer : RGB32_Display_Buffer) return Image_IO.Image_Data is
      Result : Image_IO.Image_Data
        (0 .. Display_Height - 1, 0 .. Display_Width - 1);
   begin
      for Y in 0 .. Display_Height - 1 loop
         for X in 0 .. Display_Width - 1 loop
            Result (Y, X) :=
              (Red   => Image_IO.RGB_Value (Buffer (Y, X).Red),
               Green => Image_IO.RGB_Value (Buffer (Y, X).Green),
               Blue  => Image_IO.RGB_Value (Buffer (Y, X).Blue));
         end loop;
      end loop;
      return Result;
   end To_Image;

   function Image_Equal
     (Left  : Image_IO.Image_Data;
      Right : Image_IO.Image_Data) return Boolean
   is
   begin
      if Left'Length (1) /= Right'Length (1)
        or else Left'Length (2) /= Right'Length (2)
      then
         return False;
      end if;

      for Y in Left'Range (1) loop
         for X in Left'Range (2) loop
            if Left (Y, X) /= Right (Y, X) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end Image_Equal;

   procedure Reply_OK (Payload : String := "") is
   begin
      if Payload'Length = 0 then
         Put_Line (Standard_Output, "OK");
      else
         Put_Line (Standard_Output, "OK " & Payload);
      end if;
      Flush (Standard_Output);
   end Reply_OK;

   procedure Reply_ERR (Code : String; Message : String) is
   begin
      Put_Line (Standard_Output, "ERR " & Code & " " & Message);
      Flush (Standard_Output);
   end Reply_ERR;

   procedure Ensure_Engine is
   begin
      if not G_Created then
         Create (G);
         Set_Input_Reader (G, Input_Reader'Unchecked_Access);
         G_Created := True;
         ROM_Loaded := False;
         Frame_Count := 0;
      end if;
   end Ensure_Engine;

   procedure Run_Frame is
      Requested_Samples : constant := 1000;
      Generated_Samples : Natural;
      Frame_Finished    : Boolean := False;
   begin
      while not Frame_Finished loop
         Run_For (G, Requested_Samples, Generated_Samples,
                  V_Buff'Unchecked_Access, A_Buff'Unchecked_Access,
                  Frame_Finished);
      end loop;
      Frame_Count := Frame_Count + 1;
   end Run_Frame;

   function Next_Token (Line : String; Pos : in out Positive) return String is
      First : Positive;
      Last  : Natural;
   begin
      while Pos <= Line'Last and then Line (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      if Pos > Line'Last then
         return "";
      end if;

      if Line (Pos) = '"' then
         First := Pos + 1;
         Pos := First;
         while Pos <= Line'Last and then Line (Pos) /= '"' loop
            Pos := Pos + 1;
         end loop;
         Last := Pos - 1;
         if Pos <= Line'Last then
            Pos := Pos + 1;
         end if;
         if Last < First then
            return "";
         end if;
         return Line (First .. Last);
      end if;

      First := Pos;
      while Pos <= Line'Last and then Line (Pos) /= ' ' loop
         Pos := Pos + 1;
      end loop;
      Last := Pos - 1;
      return Line (First .. Last);
   end Next_Token;

   function Parse_Natural (S : String; Value : out Natural) return Boolean is
      Clean : constant String := Trim (S, Both);
   begin
      if Clean = "" then
         return False;
      end if;

      Value := Natural'Value (Clean);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Parse_Natural;

   function Parse_Hex_U8 (S : String; Value : out Unsigned_8) return Boolean is
      Clean : constant String := Trim (S, Both);
      Acc   : Unsigned_16 := 0;
      Digit : Unsigned_16;
   begin
      if Clean = "" then
         return False;
      end if;

      for C of Clean loop
         if C in '0' .. '9' then
            Digit := Unsigned_16 (Character'Pos (C) - Character'Pos ('0'));
         elsif C in 'a' .. 'f' then
            Digit := Unsigned_16 (10 + Character'Pos (C) - Character'Pos ('a'));
         elsif C in 'A' .. 'F' then
            Digit := Unsigned_16 (10 + Character'Pos (C) - Character'Pos ('A'));
         else
            return False;
         end if;

         Acc := Shift_Left (Acc, 4) + Digit;
         if Acc > 16#FF# then
            return False;
         end if;
      end loop;

      Value := Unsigned_8 (Acc);
      return True;
   end Parse_Hex_U8;

begin
   Create (Dev_Null, Out_File, "/dev/null");
   Set_Output (Dev_Null);

   while not End_Of_File loop
      declare
         Raw_Line  : constant String := Get_Line;
         Line      : constant String := Trim (Raw_Line, Both);
         Pos       : Positive := 1;
         Command   : constant String := To_Upper (Next_Token (Line, Pos));
      begin
         if Command = "" then
            Reply_ERR ("BAD_CMD", "Empty command");

         elsif Command = "PING" then
            Reply_OK;

         elsif Command = "QUIT" then
            Reply_OK;
            exit;

         elsif Command = "RESET" then
            Ensure_Engine;
            Reset (G);
            Harness_Input.Clear (Input_Reader);
            Frame_Count := 0;
            Reply_OK;

         elsif Command = "LOAD" then
            declare
               Rom_Path : constant String := Next_Token (Line, Pos);
            begin
               if Rom_Path = "" then
                  Reply_ERR ("BAD_ARGS", "LOAD requires ROM path");
               else
                  Ensure_Engine;
                  Load_ROM (G, Rom_Path);
                  Harness_Input.Clear (Input_Reader);
                  Frame_Count := 0;
                  ROM_Loaded := True;
                  Reply_OK;
               end if;
            exception
               when E : others =>
                  Reply_ERR ("ROM", Exception_Message (E));
            end;

         elsif Command = "PRESS" or else Command = "RELEASE" then
            declare
               Button_Text : constant String := Next_Token (Line, Pos);
               Button      : Harness_Input.Button_Name;
               Pressed     : constant Boolean := (Command = "PRESS");
            begin
               if Button_Text = "" then
                  Reply_ERR ("BAD_ARGS", Command & " requires button name");
               elsif not Harness_Input.Parse_Button (Button_Text, Button) then
                  Reply_ERR ("BAD_ARGS", "Unknown button: " & Button_Text);
               else
                  Harness_Input.Set_Button (Input_Reader, Button, Pressed);
                  Reply_OK;
               end if;
            end;

         elsif Command = "SET_INPUT" then
            declare
               Mask_Text : constant String := Next_Token (Line, Pos);
               Mask      : Unsigned_8;
            begin
               if not Parse_Hex_U8 (Mask_Text, Mask) then
                  Reply_ERR ("BAD_ARGS", "SET_INPUT requires 8-bit hex mask");
               else
                  Harness_Input.Set_State_From_Mask (Input_Reader, Mask);
                  Reply_OK;
               end if;
            end;

         elsif Command = "RUN" then
            declare
               Frames_Text : constant String := Next_Token (Line, Pos);
               Frames      : Natural;
            begin
               if not ROM_Loaded then
                  Reply_ERR ("BAD_STATE", "ROM not loaded");
               elsif not Parse_Natural (Frames_Text, Frames) then
                  Reply_ERR ("BAD_ARGS", "RUN requires natural frame count");
               else
                  for I in 1 .. Frames loop
                     Run_Frame;
                  end loop;
                  Reply_OK;
               end if;
            end;

         elsif Command = "FRAME_INDEX" then
            Reply_OK (Trim (Natural'Image (Frame_Count), Both));

         elsif Command = "SAVE_FRAME" then
            declare
               Output_Path : constant String := Next_Token (Line, Pos);
            begin
               if not ROM_Loaded then
                  Reply_ERR ("BAD_STATE", "ROM not loaded");
               elsif Output_Path = "" then
                  Reply_ERR ("BAD_ARGS", "SAVE_FRAME requires output path");
               else
                  Image_IO.Operations.Write_BMP (Output_Path, To_Image (V_Buff));
                  Reply_OK;
               end if;
            exception
               when E : others =>
                  Reply_ERR ("IMAGE", Exception_Message (E));
            end;

         elsif Command = "MATCH_FRAME" then
            declare
               Ref_Path : constant String := Next_Token (Line, Pos);
               Ref      : Image_IO.Holders.Handle;
               Match    : Boolean;
            begin
               if not ROM_Loaded then
                  Reply_ERR ("BAD_STATE", "ROM not loaded");
               elsif Ref_Path = "" then
                  Reply_ERR ("BAD_ARGS", "MATCH_FRAME requires reference image path");
               else
                  Image_IO.Operations.Read (Ref_Path, Ref);
                  if Image_IO.Holders.Is_Empty (Ref) then
                     Reply_ERR ("IMAGE", "Unable to read reference image");
                  else
                     Match := Image_Equal
                       (To_Image (V_Buff), Image_IO.Holders.Value (Ref));
                     if Match then
                        Reply_OK ("1");
                     else
                        Reply_OK ("0");
                     end if;
                  end if;
               end if;
            exception
               when E : others =>
                  Reply_ERR ("IMAGE", Exception_Message (E));
            end;

         elsif Command = "FIND_MATCH" then
            declare
               Ref_Path       : constant String := Next_Token (Line, Pos);
               Max_Frames_Str : constant String := Next_Token (Line, Pos);
               Max_Frames     : Natural;
               Ref            : Image_IO.Holders.Handle;
               Found_At       : Integer := -1;
            begin
               if not ROM_Loaded then
                  Reply_ERR ("BAD_STATE", "ROM not loaded");
               elsif Ref_Path = "" then
                  Reply_ERR ("BAD_ARGS", "FIND_MATCH requires reference image path");
               elsif not Parse_Natural (Max_Frames_Str, Max_Frames) then
                  Reply_ERR ("BAD_ARGS", "FIND_MATCH requires natural max_frames");
               else
                  Image_IO.Operations.Read (Ref_Path, Ref);
                  if Image_IO.Holders.Is_Empty (Ref) then
                     Reply_ERR ("IMAGE", "Unable to read reference image");
                  else
                     if Image_Equal
                       (To_Image (V_Buff), Image_IO.Holders.Value (Ref))
                     then
                        Found_At := 0;
                     else
                        for I in 1 .. Max_Frames loop
                           Run_Frame;
                           if Image_Equal
                             (To_Image (V_Buff), Image_IO.Holders.Value (Ref))
                           then
                              Found_At := Integer (I);
                              exit;
                           end if;
                        end loop;
                     end if;

                     Reply_OK (Trim (Integer'Image (Found_At), Both));
                  end if;
               end if;
            exception
               when E : others =>
                  Reply_ERR ("IMAGE", Exception_Message (E));
            end;

         else
            Reply_ERR ("BAD_CMD", "Unknown command: " & Command);
         end if;
      exception
         when End_Error =>
            exit;
         when E : others =>
            Reply_ERR ("INTERNAL", Exception_Message (E));
      end;
   end loop;

   Close (Dev_Null);

exception
   when E : others =>
      begin
         Put_Line (Standard_Output,
                   "ERR FATAL " & Exception_Message (E));
      exception
         when others =>
            null;
      end;
end Gade_Testd;
