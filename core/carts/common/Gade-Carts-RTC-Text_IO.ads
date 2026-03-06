private with Ada.Calendar;

package Gade.Carts.RTC.Text_IO is

   --  Debug helpers for RTC state output.

   procedure Print (Clk : Clock);

private
   use Ada.Calendar;

   function Days (D : Duration) return Natural;
   function Truncate (D : Duration) return Day_Duration;
   function Maximum (Left, Right : Duration) return Duration;

   function Duration_Image (D : Duration) return String;
   function Counter_Image (C : Counter) return String;

end Gade.Carts.RTC.Text_IO;
