with Ada.Strings.Fixed;

package body Testd.Protocol is

   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Interfaces;

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

   function Parse_Hex_U8
     (S : String; Value : out Interfaces.Unsigned_8) return Boolean
   is
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

end Testd.Protocol;
