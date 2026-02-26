with Ada.Text_IO;

with Harness_Input;
with Gade.Interfaces;
with Gade.Video_Buffer;
with Gade.Audio_Buffer;

package Testd_Session is

   type Session is limited private;

   procedure Initialize (S : in out Session);
   procedure Finalize (S : in out Session);

   --  Process one protocol line. Returns True when caller should terminate.
   function Process_Line
     (S        : in out Session;
      Raw_Line : String) return Boolean;

   procedure Reply_Fatal (Message : String);

private

   procedure Reply_OK (Payload : String := "");
   procedure Reply_ERR (Code : String; Message : String);

   type Session is limited record
      Dev_Null : Ada.Text_IO.File_Type;

      G : Gade.Interfaces.Gade_Type;
      G_Created : Boolean := False;
      ROM_Loaded : Boolean := False;

      V_Buff : aliased Gade.Video_Buffer.RGB32_Display_Buffer;
      A_Buff : aliased Gade.Audio_Buffer.Audio_Buffer_Type;

      Input_Reader : aliased Harness_Input.Manual_Input_Reader;

      Frame_Count : Natural := 0;
   end record;

end Testd_Session;
