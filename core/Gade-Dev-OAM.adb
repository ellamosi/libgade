package body Gade.Dev.OAM is

   procedure Reset
     (OAM : in out OAM_Type) is
   begin
      OAM.Map.Space := (others => 0);
   end Reset;

   procedure Read
     (OAM     : in out OAM_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte) is
   begin
      Value := OAM.Map.Space(Address);
   end Read;

   procedure Write
     (OAM     : in out OAM_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : Byte) is
   begin
      OAM.Map.Space(Address) := Value;
   end Write;

end Gade.Dev.OAM;
