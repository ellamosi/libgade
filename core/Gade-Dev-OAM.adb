package body Gade.Dev.OAM is

   overriding
   procedure Reset
     (OAM : in out OAM_Type) is
   begin
      OAM.Map.Space := (others => 0);
   end Reset;

   overriding
   procedure Read
     (OAM     : in out OAM_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte) is
      pragma Unreferenced (GB);
   begin
      Value := OAM.Map.Space (Address);
   end Read;

   overriding
   procedure Write
     (OAM     : in out OAM_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : Byte) is
      pragma Unreferenced (GB);
   begin
      OAM.Map.Space (Address) := Value;
   end Write;

end Gade.Dev.OAM;
