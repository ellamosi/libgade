private with Ada.Calendar;

package Gade.Carts.RTC.Text_IO is

   --  This is a set of methods that were used to help debug the RTC
   --  implementation during development. Keeping them around as a reference
   --  for when a proper logging system is implemented.

   procedure Print (Clk : Clock);

private
   use Ada.Calendar;

   function Days (D : Duration) return Natural;
   function Truncate (D : Duration) return Day_Duration;
   function Maximum (Left, Right : Duration) return Duration;

   function Duration_Image (D : Duration) return String;
   function Counter_Image (C : Counter) return String;

end Gade.Carts.RTC.Text_IO;
