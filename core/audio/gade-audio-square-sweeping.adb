with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Audio.Square.Sweeping is

   overriding
   procedure Extended_Reset (Ch : in out Sweeping_Square_Channel) is
   begin
      Ch.Sweep_Negate := False;
      Put_Line ("Extended Reset");
      Setup (Ch.Sweep_Timer);
   end Extended_Reset;

   overriding
   function Get_Read_Masks (Ch : Sweeping_Square_Channel) return Register_Masks
   is
      pragma Unreferenced (Ch);
   begin
      return Read_Masks;
   end Get_Read_Masks;

   overriding
   procedure Extended_Trigger (Ch : in out Sweeping_Square_Channel)
                               renames Trigger_Frequency_Sweep;

   --  During a trigger event, several things occur:
   --  - Square 1's frequency is copied to the shadow register.
   --  - The sweep timer is reloaded.
   --  - The internal enabled flag is set if either the sweep period or shift
   --    are non-zero, cleared otherwise.
   --  - If the sweep shift is non-zero, frequency calculation and the overflow
   --    check are performed immediately.
   --
   --  Square 1's frequency can be modified via NR13 and NR14 while
   --  sweep is active, but the shadow frequency won't be affected so
   --  the next time the sweep updates the channel's frequency this
   --  modification will be lost.
   procedure Trigger_Frequency_Sweep (Ch : in out Sweeping_Square_Channel) is
   begin
      if Ch.IO.Sweep_Period > 0 and Ch.IO.Shift > 0 then
         Ch.Sweep_Shift := Ch.IO.Shift;

         Setup (Ch.Sweep_Timer, Natural (Ch.IO.Sweep_Period));
         Start (Ch.Sweep_Timer);

         Ch.Shadow_Frequency := Ch.IO.Frequency;
         Ch.Sweep_Negate := Ch.IO.Negate;

         --  if Shift /= 0 then Frequency_Sweep_Step (Ch); end if;
      else
         Stop (Ch.Sweep_Timer);
      end if;
   end Trigger_Frequency_Sweep;

   procedure Frequency_Sweep_Shift (Ch : in out Sweeping_Square_Channel) is
      New_Freq : Frequency_Type;
      Shifted : Integer;
      Out_Of_Range : Boolean;
   begin
      --  Frequency calculation consists of taking the value in the frequency
      --  shadow register, shifting it right by sweep shift, optionally
      --  negating the value, and summing this with the frequency shadow
      --  register to produce a new frequency.
      Shifted := (Integer (Ch.Shadow_Frequency) / 2 ** Natural (Ch.Sweep_Shift));
      if Ch.Sweep_Negate then
         --  Down
         New_Freq := Ch.Shadow_Frequency - Frequency_Type (Shifted);
         Out_Of_Range := New_Freq > Ch.Shadow_Frequency;
      else
         --  Up
         New_Freq := Ch.Shadow_Frequency + Frequency_Type (Shifted);
         Out_Of_Range := New_Freq < Ch.Shadow_Frequency;
      end if;

      if Out_Of_Range then
         Stop (Ch.Sweep_Timer);
         Ch.Disable;
      else
         Ch.Shadow_Frequency := New_Freq;
         Reset (Ch.Sweep_Timer);
         Put_Line ("Sweep New Freq (Dec):" & New_Freq'Img);
         Set_Frequency (Ch, New_Freq);
      end if;
   end Frequency_Sweep_Shift;

   procedure Frequency_Sweep_Step (Ch : in out Sweeping_Square_Channel) is
   begin
      Tick (Ch.Sweep_Timer);
      if Has_Finished (Ch.Sweep_Timer) then Frequency_Sweep_Shift (Ch); end if;
   end Frequency_Sweep_Step;

end Gade.Audio.Square.Sweeping;
