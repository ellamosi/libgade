with Interfaces;

package Testd_Session.Protocol is

   function Next_Token (Line : String; Pos : in out Positive) return String;

   function Parse_Natural (S : String; Value : out Natural) return Boolean;

   function Parse_Hex_U8
     (S : String; Value : out Interfaces.Unsigned_8) return Boolean;

end Testd_Session.Protocol;
