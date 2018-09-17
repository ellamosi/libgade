with Ada.Calendar.Conversions; use Ada.Calendar.Conversions;
with Interfaces.C;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;

package body Gade.Cart.RTC is

   procedure Sanitize (FD : in out File_Data) is
      Now : constant Timestamp_64 :=
        Timestamp_64 (To_Unix_Time (Ada.Calendar.Clock));
   begin
      if FD.Saved_At > Now then FD.Saved_At := Now; end if;
      FD.Time.Seconds := FD.Time.Seconds mod 60;
      FD.Time.Minutes := FD.Time.Minutes mod 60;
      FD.Time.Hours   := FD.Time.Hours   mod 24;
   end Sanitize;

   function Elapsed (Time : File_Time_Info) return Duration is
      Days : Natural;
      Seconds : Natural;
   begin
      Days :=
        Natural (Time.Days_Low) +
        Natural (Time.Days_Top) * 16#100#;
      Seconds :=
        Natural (Time.Seconds) +
        Natural (Time.Minutes) * 60 +
        Natural (Time.Hours) * 60 * 60 +
        Days * 24 * 60 * 60;
      return Duration (Seconds);
   end Elapsed;

   function To_Counter (Time : File_Time_Info) return Counter is
      Result : Counter;
   begin
      Result.Span := Elapsed (Time);
      Result.Halted := Time.Halted;
      Result.Overflown := Time.Carry;
      return Result;
   end To_Counter;

   function To_File_Time (C : Counter) return File_Time_Info is
      Result : File_Time_Info;
      Seconds : constant Natural := Natural (C.Span);
      Minutes : constant Natural := Seconds / 60;
      Hours   : constant Natural := Minutes / 60;
      Days    : constant Natural := Hours / 24;
   begin
      --  TODO: Do this in a re-usable method
      Result.Seconds  := Unsigned_32 (Seconds mod 60);
      Result.Minutes  := Unsigned_32 (Minutes mod 60);
      Result.Hours    := Unsigned_32 (Hours mod 24);
      Result.Days_Low := Unsigned_32 (Days mod 256);
      Result.Days_Top := (if Days > 255 then 1 else 0);
      Result.Carry  := C.Overflown;
      Result.Halted := C.Halted;
      return Result;
   end To_File_Time;

   function Days (D : Duration) return Natural is
   begin
      return Natural (D / Seconds_Day);
   end Days;

   function Truncate (D : Duration) return Day_Duration is
      Day_Count : constant Natural := Days (D);
   begin
      return Maximum (D - Day_Count * Seconds_Day, 0.0);
   end Truncate;

   function Maximum (Left, Right : Duration) return Duration is
   begin
      return (if Left > Right then Left else Right);
   end Maximum;

   function Duration_Image (D : Duration) return String is
      Days_Img : constant String := Days (D)'Img;
   begin
      return
        Days_Img (Days_Img'First + 1 .. Days_Img'Last) &
        "d " & Image (Truncate (D));
   end Duration_Image;

   function Counter_Image (C : Counter) return String is
      Halted    : String := "H: ";
      Overflown : String := "O: ";
   begin
      Halted (Halted'Last)       := (if C.Halted then 't' else 'f');
      Overflown (Overflown'Last) := (if C.Overflown then 't' else 'f');
      return Duration_Image (C.Span) & ' ' & Overflown & ' ' & Halted;
   end Counter_Image;

   procedure Print (Clk : Clock);
   procedure Print (Clk : Clock) is
   begin
      Ada.Text_IO.Put_Line ("  Elapsed:    " & Counter_Image (Clk.Elapsed));
      Ada.Text_IO.Put_Line ("  Latched:    " & Counter_Image (Clk.Latched));
      Ada.Text_IO.Put_Line ("  Updated_At: " & Image (Clk.Updated_At));
   end Print;

   function To_Clock (FD : File_Data) return Clock is
      Clk : Clock;
   begin
      Clk.Updated_At := To_Ada_Time (Interfaces.C.long (FD.Saved_At));
      Clk.Elapsed    := To_Counter (FD.Time);
      Clk.Latched    := To_Counter (FD.Latched);
      return Clk;
   end To_Clock;

   function To_File_Data (Clk : Clock) return File_Data is
      FD : File_Data;
   begin
      FD.Content_64 := (others => 0);
      FD.Time := To_File_Time (Clk.Elapsed);
      FD.Latched := To_File_Time (Clk.Latched);
      FD.Saved_At := Timestamp_64 (To_Unix_Time (Clk.Updated_At));
      return FD;
   end To_File_Data;

   procedure Reset (Clk : out Clock) is
   begin
      Clk.Elapsed    := (Span => 0.0, Halted => False, Overflown => False);
      Clk.Latched    := (Span => 0.0, Halted => False, Overflown => False);
      Clk.Updated_At := Ada.Calendar.Clock;
   end Reset;

   procedure Refresh (Clk : in out Clock) is
      Now : constant Time := Ada.Calendar.Clock;
   begin
      if not Clk.Elapsed.Halted then
         Clk.Elapsed.Span := Now - Clk.Updated_At + Clk.Elapsed.Span;
         if Natural (Clk.Elapsed.Span) > Max_Span then
            Clk.Elapsed.Overflown := True;
            Clk.Elapsed.Span := Duration (Natural (Clk.Elapsed.Span) mod Max_Span);
         end if;
      end if;
      Clk.Updated_At := Now;
   end Refresh;

   procedure Read
     (Clk : Clock;
      Seconds, Minutes, Hours, Days_Low, Days_High : out Byte)
   is
      Span_Seconds : constant Natural := Natural (Clk.Elapsed.Span);
      Span_Minutes : constant Natural := Span_Seconds / 60;
      Span_Hours   : constant Natural := Span_Minutes / 60;
      Span_Days    : constant Natural := Span_Hours / 24;
   begin
      Seconds   := Byte (Span_Seconds mod 60);
      Minutes   := Byte (Span_Minutes mod 60);
      Hours     := Byte (Span_Hours mod 24);
      Days_Low  := Byte (Span_Days mod 256);
      Days_High := 0; --  TODO
   end Read;

   procedure Read (File : File_Type; RTC_Data : out File_Data) is
      Input_Stream : Stream_Access;
      File_Size    : Count;
      Tail_Size    : Count;
   begin
      File_Size := Size (File);
      Tail_Size := File_Size mod 8192;
      Set_Index (File, File_Size - Tail_Size);
      Input_Stream := Ada.Streams.Stream_IO.Stream (File);
      if Tail_Size >= File_Data_64_Size then
         File_Data_Buffer_64'Read (Input_Stream, RTC_Data.Content_64);
      else
         RTC_Data.Saved_At := 0;
         File_Data_Buffer_32'Read (Input_Stream, RTC_Data.Content_32);
      end if;
   end Read;

   procedure Load
     (Path : String;
      Clk  : out Clock)
   is
      File     : File_Type;
      RTC_Data : File_Data;
   begin
      Reset (Clk);
      Open (File, In_File, Path);
      Read (File, RTC_Data);
      Close (File);
      Sanitize (RTC_Data);
      Clk := To_Clock (RTC_Data);
      Ada.Text_IO.Put_Line ("Loaded RTC:");
      Print (Clk);
   exception
      when Name_Error =>
         Ada.Text_IO.Put_Line ("No RTC file found");
      when Error : others =>
         Ada.Text_IO.Put_Line ("Unable to read RTC file");
         Ada.Text_IO.Put_Line (Exception_Information (Error));
         Close (File);
   end Load;

   procedure Save
     (Path : String;
      Clk  : Clock)
   is
      File          : File_Type;
      Output_Stream : Stream_Access;
      RTC_Data      : File_Data;
      Clk2          : Clock;
   begin
      Clk2 := Clk;
      Refresh (Clk2);
      Ada.Text_IO.Put_Line ("Saving RTC:");
      Print (Clk2);
      RTC_Data := To_File_Data (Clk2);
      Create (File, Out_File, Path);
      Output_Stream := Ada.Streams.Stream_IO.Stream (File);
      File_Data_Buffer_64'Write (Output_Stream, RTC_Data.Content_64);
      Close (File);
   exception
      when Error : others =>
         Ada.Text_IO.Put_Line ("Unable to write RTC file");
         Ada.Text_IO.Put_Line (Exception_Information (Error));
   end Save;

end Gade.Cart.RTC;
