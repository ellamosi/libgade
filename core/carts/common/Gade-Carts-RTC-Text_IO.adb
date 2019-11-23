with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

package body Gade.Carts.RTC.Text_IO is

   procedure Print (Clk : Clock) is
   begin
      Ada.Text_IO.Put_Line ("  Elapsed:    " & Counter_Image (Clk.Elapsed));
      Ada.Text_IO.Put_Line ("  Latched:    " & Counter_Image (Clk.Latched));
   end Print;

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
      Overflown (Overflown'Last) := (if C.Carry then 't' else 'f');
      return Duration_Image (C.Span) & ' ' & Overflown & ' ' & Halted;
   end Counter_Image;

end Gade.Carts.RTC.Text_IO;
