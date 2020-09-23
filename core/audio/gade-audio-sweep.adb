with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Audio.Sweep is

   overriding
   procedure Extended_Reset (Ch : in out Sweep_Channel) is
   begin
      Ch.Sweep_Negate := False;
      Put_Line ("Extended Reset");
      Setup (Ch.Sweep_Timer);
   end Extended_Reset;

   overriding
   procedure NRx0_Read
     (Ch    : in out Sweep_Channel;
      Value : out Byte)
   is
   begin
      Value := Ch.IO.Space;
   end NRx0_Read;

   overriding
   procedure NRx0_Write
     (Ch    : in out Sweep_Channel;
      Value : Byte)
   is
   begin
      Ch.IO.Space := Value or Sweep_IO_Mask;
      --  TODO, set internal sweep state ?
   end NRx0_Write;

   overriding
   procedure Extended_Trigger
     (Ch        : in out Sweep_Channel;
      Frequency : Frequency_Type)
   is
   begin
      --  TODO: Revise signature
      Trigger_Frequency_Sweep (Ch, Frequency, Ch.IO.Period, Ch.IO.Negate, Ch.IO.Shift);
   end Extended_Trigger;

   --  During a trigger event, several things occur:
   --  - Square 1's frequency is copied to the shadow register.
   --  - The sweep timer is reloaded.
   --  - The internal enabled flag is set if either the sweep period or shift
   --    are non-zero, cleared otherwise.
   --  - If the sweep shift is non-zero, frequency calculation and the overflow
   --    check are performed immediately.
   procedure Trigger_Frequency_Sweep
     (Ch        : in out Sweep_Channel;
      Frequency : Frequency_Type;
      Period    : Sweep_Period_Type;
      Negate    : Boolean;
      Shift     : Sweep_Shift_Type)
   is
   begin
      if Period > 0 and Shift > 0 then
         Ch.Sweep_Shift := Shift;
         --  Enable timer
         Setup (Ch.Sweep_Timer, Natural (Period));
         Start (Ch.Sweep_Timer);

         Ch.Shadow_Frequency := Frequency;
         Ch.Sweep_Negate := Negate;

         --  if Shift /= 0 then Frequency_Sweep_Step (Ch); end if;
      else
         Stop (Ch.Sweep_Timer);
      end if;
   end Trigger_Frequency_Sweep;

   procedure Frequency_Sweep_Shift (Ch : in out Sweep_Channel) is
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
         --  TODO: Update register
      end if;
   end Frequency_Sweep_Shift;

   procedure Frequency_Sweep_Step (Ch : in out Sweep_Channel) is
   begin
      Tick (Ch.Sweep_Timer);
      if Has_Finished (Ch.Sweep_Timer) then Frequency_Sweep_Shift (Ch); end if;
   end Frequency_Sweep_Step;


   type IO_Select is (A_IO, B_IO);
   type Shared_IO (S : IO_Select) is record
      R1 : Byte;
      case S is
         when A_IO => null;
         when B_IO => R0 : Byte;
      end case;
   end record;
   for Shared_IO use record
      S  at 0 range 0 .. 15;
      R0 at 2 range 0 .. 7;
      R1 at 3 range 0 .. 7;
   end record;

   generic
      S : IO_Select;
   package Common is
      type Common_Device is tagged record
         IO : Shared_IO (S);
      end record;

      procedure Test (Dev : in out Common_Device);
   end Common;

   package body Common is
      procedure Test (Dev : in out Common_Device) is
      begin
         Dev.IO.R0 := 0; -- Will trigger warning upon generic instantiation with IO_Select (A_IO)
         Dev.IO.R1 := 0; -- Will work fine on either generic instantiation
      end Test;
   end Common;

   package A is

      package Common_A is new Common (A_IO);

      type Device_A is new Common_A.Common_Device with null record;

      overriding procedure Test (Dev : in out Device_A);

   end A;

   package body A is
      overriding procedure Test (Dev : in out Device_A) is
      begin
         Dev.IO.R0 := 0; -- Triggers compiler warning
         Dev.IO.R1 := 0; -- Works fine
      end Test;
   end A;

   package B is

      package Common_B is new Common (B_IO);

      type Device_B is new Common_B.Common_Device with null record;

      overriding procedure Test (Dev : in out Device_B);

   end B;

   package body B is
      overriding procedure Test (Dev : in out Device_B) is
      begin
         Dev.IO.R0 := 0; -- Works fine
         Dev.IO.R1 := 0; -- Works fine
      end Test;
   end B;

end Gade.Audio.Sweep;
