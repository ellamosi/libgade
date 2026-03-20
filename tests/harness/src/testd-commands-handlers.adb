with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Interfaces;              use Interfaces;

with Image_IO.Holders;
with Image_IO.Operations;

with Testd.Input;
with Testd.Protocol;

with Gade.Interfaces; use Gade.Interfaces;

package body Testd.Commands.Handlers is

   procedure Reset (S : in out Session) is
   begin
      Ensure_Engine (S);
      Reset (S.G);
      Testd.Input.Clear (S.Input_Reader);
      S.Frame_Count := 0;
      Reply_OK;
   end Reset;

   procedure Load (S : in out Session; Line : String; Pos : in out Positive) is
      Rom_Path : constant String := Testd.Protocol.Next_Token (Line, Pos);
   begin
      if Rom_Path = "" then
         Reply_ERR ("BAD_ARGS", "LOAD requires ROM path");
      else
         Ensure_Engine (S);
         Load_ROM (S.G, Rom_Path);
         Testd.Input.Clear (S.Input_Reader);
         S.Frame_Count := 0;
         S.ROM_Loaded := True;
         Reply_OK;
      end if;
   exception
      when E : others =>
         Reply_ERR ("ROM", Exception_Message (E));
   end Load;

   procedure Press_Release
     (S       : in out Session;
      Line    : String;
      Pos     : in out Positive;
      Command : String;
      Pressed : Boolean)
   is
      Button_Text : constant String := Testd.Protocol.Next_Token (Line, Pos);
      Button      : Testd.Input.Button_Name;
   begin
      if Button_Text = "" then
         Reply_ERR ("BAD_ARGS", Command & " requires button name");
      elsif not Testd.Input.Parse_Button (Button_Text, Button) then
         Reply_ERR ("BAD_ARGS", "Unknown button: " & Button_Text);
      else
         Testd.Input.Set_Button (S.Input_Reader, Button, Pressed);
         Reply_OK;
      end if;
   end Press_Release;

   procedure Set_Input
     (S : in out Session; Line : String; Pos : in out Positive)
   is
      Mask_Text : constant String := Testd.Protocol.Next_Token (Line, Pos);
      Mask      : Unsigned_8;
   begin
      if not Testd.Protocol.Parse_Hex_U8 (Mask_Text, Mask) then
         Reply_ERR ("BAD_ARGS", "SET_INPUT requires 8-bit hex mask");
      else
         Testd.Input.Set_State_From_Mask (S.Input_Reader, Mask);
         Reply_OK;
      end if;
   end Set_Input;

   procedure Run (S : in out Session; Line : String; Pos : in out Positive) is
      Frames_Text : constant String := Testd.Protocol.Next_Token (Line, Pos);
      Frames      : Natural;
   begin
      if not Ensure_ROM_Loaded (S) then
         return;
      elsif not Testd.Protocol.Parse_Natural (Frames_Text, Frames) then
         Reply_ERR ("BAD_ARGS", "RUN requires natural frame count");
      else
         for I in 1 .. Frames loop
            Run_Frame (S);
         end loop;
         Reply_OK;
      end if;
   end Run;

   procedure Save_Frame
     (S : in out Session; Line : String; Pos : in out Positive)
   is
      Output_Path : constant String := Testd.Protocol.Next_Token (Line, Pos);
      Lower_Path  : String := Output_Path;
   begin
      if not Ensure_ROM_Loaded (S) then
         return;
      elsif Output_Path = "" then
         Reply_ERR ("BAD_ARGS", "SAVE_FRAME requires output path");
      else
         for I in Lower_Path'Range loop
            Lower_Path (I) := To_Lower (Lower_Path (I));
         end loop;

         if Lower_Path'Length >= 4
           and then Lower_Path (Lower_Path'Last - 3 .. Lower_Path'Last)
                    = ".png"
         then
            Image_IO.Operations.Write_PNG (Output_Path, To_Image (S.V_Buff));
         else
            Image_IO.Operations.Write_BMP (Output_Path, To_Image (S.V_Buff));
         end if;
         Reply_OK;
      end if;
   exception
      when E : others =>
         Reply_ERR ("IMAGE", Exception_Message (E));
   end Save_Frame;

   procedure Match_Frame
     (S : in out Session; Line : String; Pos : in out Positive)
   is
      Ref_Path : constant String := Testd.Protocol.Next_Token (Line, Pos);
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
            Match :=
              Image_Equal (To_Image (S.V_Buff), Image_IO.Holders.Value (Ref));
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
   end Match_Frame;

   procedure Find_Match
     (S : in out Session; Line : String; Pos : in out Positive)
   is
      Ref_Path       : constant String :=
        Testd.Protocol.Next_Token (Line, Pos);
      Max_Frames_Str : constant String :=
        Testd.Protocol.Next_Token (Line, Pos);
      Max_Frames     : Natural;
      Ref            : Image_IO.Holders.Handle;
      Found_At       : Integer := -1;
   begin
      if not Ensure_ROM_Loaded (S) then
         return;
      elsif Ref_Path = "" then
         Reply_ERR ("BAD_ARGS", "FIND_MATCH requires reference image path");
      elsif not Testd.Protocol.Parse_Natural (Max_Frames_Str, Max_Frames) then
         Reply_ERR ("BAD_ARGS", "FIND_MATCH requires natural max_frames");
      else
         Image_IO.Operations.Read (Ref_Path, Ref);
         if Image_IO.Holders.Is_Empty (Ref) then
            Reply_ERR ("IMAGE", "Unable to read reference image");
         else
            if Image_Equal (To_Image (S.V_Buff), Image_IO.Holders.Value (Ref))
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
   end Find_Match;

end Testd.Commands.Handlers;
