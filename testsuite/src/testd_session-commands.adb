with Ada.Exceptions;
with Ada.Strings.Fixed;
with Interfaces;
with Image_IO;
with Image_IO.Holders;
with Image_IO.Operations;

with Harness_Input;
with Gade.Interfaces;
with Gade.Video_Buffer;

with Testd_Session.Protocol;

package body Testd_Session.Commands is

   use Ada.Exceptions;
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Interfaces;
   use type Image_IO.Color_Info;
   use Gade.Interfaces;
   use Gade.Video_Buffer;

   function To_Image (Buffer : RGB32_Display_Buffer) return Image_IO.Image_Data;
   function Image_Equal
     (Left  : Image_IO.Image_Data;
      Right : Image_IO.Image_Data) return Boolean;

   procedure Ensure_Engine (S : in out Session);
   procedure Run_Frame (S : in out Session);
   function Ensure_ROM_Loaded (S : Session) return Boolean;

   procedure Handle_Reset (S : in out Session);
   procedure Handle_Load
     (S    : in out Session;
      Line : String;
      Pos  : in out Positive);
   procedure Handle_Press_Release
     (S       : in out Session;
      Line    : String;
      Pos     : in out Positive;
      Command : String;
      Pressed : Boolean);
   procedure Handle_Set_Input
     (S    : in out Session;
      Line : String;
      Pos  : in out Positive);
   procedure Handle_Run
     (S    : in out Session;
      Line : String;
      Pos  : in out Positive);
   procedure Handle_Save_Frame
     (S    : in out Session;
      Line : String;
      Pos  : in out Positive);
   procedure Handle_Match_Frame
     (S    : in out Session;
      Line : String;
      Pos  : in out Positive);
   procedure Handle_Find_Match
     (S    : in out Session;
      Line : String;
      Pos  : in out Positive);

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

   procedure Ensure_Engine (S : in out Session) is
   begin
      if not S.G_Created then
         Create (S.G);
         Set_Input_Reader (S.G, S.Input_Reader'Unchecked_Access);
         S.G_Created := True;
         S.ROM_Loaded := False;
         S.Frame_Count := 0;
      end if;
   end Ensure_Engine;

   procedure Run_Frame (S : in out Session) is
      Requested_Samples : constant := 1000;
      Generated_Samples : Natural;
      Frame_Finished    : Boolean := False;
   begin
      while not Frame_Finished loop
         Run_For (S.G, Requested_Samples, Generated_Samples,
                  S.V_Buff'Unchecked_Access, S.A_Buff'Unchecked_Access,
                  Frame_Finished);
      end loop;
      S.Frame_Count := S.Frame_Count + 1;
   end Run_Frame;

   function Ensure_ROM_Loaded (S : Session) return Boolean is
   begin
      if not S.ROM_Loaded then
         Reply_ERR ("BAD_STATE", "ROM not loaded");
         return False;
      end if;

      return True;
   end Ensure_ROM_Loaded;

   procedure Handle_Reset (S : in out Session) is
   begin
      Ensure_Engine (S);
      Reset (S.G);
      Harness_Input.Clear (S.Input_Reader);
      S.Frame_Count := 0;
      Reply_OK;
   end Handle_Reset;

   procedure Handle_Load
     (S    : in out Session;
      Line : String;
      Pos  : in out Positive)
   is
      Rom_Path : constant String := Testd_Session.Protocol.Next_Token (Line, Pos);
   begin
      if Rom_Path = "" then
         Reply_ERR ("BAD_ARGS", "LOAD requires ROM path");
      else
         Ensure_Engine (S);
         Load_ROM (S.G, Rom_Path);
         Harness_Input.Clear (S.Input_Reader);
         S.Frame_Count := 0;
         S.ROM_Loaded := True;
         Reply_OK;
      end if;
   exception
      when E : others =>
         Reply_ERR ("ROM", Exception_Message (E));
   end Handle_Load;

   procedure Handle_Press_Release
     (S       : in out Session;
      Line    : String;
      Pos     : in out Positive;
      Command : String;
      Pressed : Boolean)
   is
      Button_Text : constant String := Testd_Session.Protocol.Next_Token (Line, Pos);
      Button      : Harness_Input.Button_Name;
   begin
      if Button_Text = "" then
         Reply_ERR ("BAD_ARGS", Command & " requires button name");
      elsif not Harness_Input.Parse_Button (Button_Text, Button) then
         Reply_ERR ("BAD_ARGS", "Unknown button: " & Button_Text);
      else
         Harness_Input.Set_Button (S.Input_Reader, Button, Pressed);
         Reply_OK;
      end if;
   end Handle_Press_Release;

   procedure Handle_Set_Input
     (S    : in out Session;
      Line : String;
      Pos  : in out Positive)
   is
      Mask_Text : constant String := Testd_Session.Protocol.Next_Token (Line, Pos);
      Mask      : Unsigned_8;
   begin
      if not Testd_Session.Protocol.Parse_Hex_U8 (Mask_Text, Mask) then
         Reply_ERR ("BAD_ARGS", "SET_INPUT requires 8-bit hex mask");
      else
         Harness_Input.Set_State_From_Mask (S.Input_Reader, Mask);
         Reply_OK;
      end if;
   end Handle_Set_Input;

   procedure Handle_Run
     (S    : in out Session;
      Line : String;
      Pos  : in out Positive)
   is
      Frames_Text : constant String := Testd_Session.Protocol.Next_Token (Line, Pos);
      Frames      : Natural;
   begin
      if not Ensure_ROM_Loaded (S) then
         return;
      elsif not Testd_Session.Protocol.Parse_Natural (Frames_Text, Frames) then
         Reply_ERR ("BAD_ARGS", "RUN requires natural frame count");
      else
         for I in 1 .. Frames loop
            Run_Frame (S);
         end loop;
         Reply_OK;
      end if;
   end Handle_Run;

   procedure Handle_Save_Frame
     (S    : in out Session;
      Line : String;
      Pos  : in out Positive)
   is
      Output_Path : constant String := Testd_Session.Protocol.Next_Token (Line, Pos);
   begin
      if not Ensure_ROM_Loaded (S) then
         return;
      elsif Output_Path = "" then
         Reply_ERR ("BAD_ARGS", "SAVE_FRAME requires output path");
      else
         Image_IO.Operations.Write_BMP (Output_Path, To_Image (S.V_Buff));
         Reply_OK;
      end if;
   exception
      when E : others =>
         Reply_ERR ("IMAGE", Exception_Message (E));
   end Handle_Save_Frame;

   procedure Handle_Match_Frame
     (S    : in out Session;
      Line : String;
      Pos  : in out Positive)
   is
      Ref_Path : constant String := Testd_Session.Protocol.Next_Token (Line, Pos);
      Ref      : Image_IO.Holders.Handle;
      Match    : Boolean;
   begin
      if not Ensure_ROM_Loaded (S) then
         return;
      elsif Ref_Path = "" then
         Reply_ERR ("BAD_ARGS", "MATCH_FRAME requires reference image path");
      else
         Image_IO.Operations.Read (Ref_Path, Ref);
         if Image_IO.Holders.Is_Empty (Ref) then
            Reply_ERR ("IMAGE", "Unable to read reference image");
         else
            Match := Image_Equal
              (To_Image (S.V_Buff), Image_IO.Holders.Value (Ref));
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
   end Handle_Match_Frame;

   procedure Handle_Find_Match
     (S    : in out Session;
      Line : String;
      Pos  : in out Positive)
   is
      Ref_Path       : constant String := Testd_Session.Protocol.Next_Token (Line, Pos);
      Max_Frames_Str : constant String := Testd_Session.Protocol.Next_Token (Line, Pos);
      Max_Frames     : Natural;
      Ref            : Image_IO.Holders.Handle;
      Found_At       : Integer := -1;
   begin
      if not Ensure_ROM_Loaded (S) then
         return;
      elsif Ref_Path = "" then
         Reply_ERR ("BAD_ARGS", "FIND_MATCH requires reference image path");
      elsif not Testd_Session.Protocol.Parse_Natural (Max_Frames_Str, Max_Frames) then
         Reply_ERR ("BAD_ARGS", "FIND_MATCH requires natural max_frames");
      else
         Image_IO.Operations.Read (Ref_Path, Ref);
         if Image_IO.Holders.Is_Empty (Ref) then
            Reply_ERR ("IMAGE", "Unable to read reference image");
         else
            if Image_Equal
              (To_Image (S.V_Buff), Image_IO.Holders.Value (Ref))
            then
               Found_At := 0;
            else
               for I in 1 .. Max_Frames loop
                  Run_Frame (S);
                  if Image_Equal
                    (To_Image (S.V_Buff), Image_IO.Holders.Value (Ref))
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
   end Handle_Find_Match;

   function Dispatch_Command
     (S       : in out Session;
      Command : String;
      Line    : String;
      Pos     : in out Positive) return Boolean
   is
   begin
      if Command = "PING" then
         Reply_OK;

      elsif Command = "QUIT" then
         Reply_OK;
         return True;

      elsif Command = "RESET" then
         Handle_Reset (S);

      elsif Command = "LOAD" then
         Handle_Load (S, Line, Pos);

      elsif Command = "PRESS" then
         Handle_Press_Release (S, Line, Pos, Command, Pressed => True);

      elsif Command = "RELEASE" then
         Handle_Press_Release (S, Line, Pos, Command, Pressed => False);

      elsif Command = "SET_INPUT" then
         Handle_Set_Input (S, Line, Pos);

      elsif Command = "RUN" then
         Handle_Run (S, Line, Pos);

      elsif Command = "FRAME_INDEX" then
         Reply_OK (Trim (Natural'Image (S.Frame_Count), Both));

      elsif Command = "SAVE_FRAME" then
         Handle_Save_Frame (S, Line, Pos);

      elsif Command = "MATCH_FRAME" then
         Handle_Match_Frame (S, Line, Pos);

      elsif Command = "FIND_MATCH" then
         Handle_Find_Match (S, Line, Pos);

      else
         Reply_ERR ("BAD_CMD", "Unknown command: " & Command);
      end if;

      return False;
   end Dispatch_Command;

end Testd_Session.Commands;
