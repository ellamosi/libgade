package body Gade.Dev.External_RAM is

   overriding
   procedure Reset (External_RAM : in out External_RAM_Type) is
   begin
      External_RAM.Enabled := False;
      --  TODO: This should only initialize necessary banks based on ROM info
      for Bank of External_RAM.Banks loop
         Bank := new RAM_Bank;
         Bank.all := (others => 0);
      end loop;
      External_RAM.Current_Bank := External_RAM.Banks (0);
   end Reset;

   procedure Set_Enabled
     (External_RAM : in out External_RAM_Type;
      Enabled      : Boolean) is
   begin
      External_RAM.Enabled := Enabled;
   end Set_Enabled;

   procedure Switch_Banks
     (External_RAM : in out External_RAM_Type;
      Bank         : External_RAM_Bank_Range) is
   begin
      External_RAM.Current_Bank := External_RAM.Banks (Bank);
   end Switch_Banks;

   overriding
   procedure Read
     (External_RAM : in out External_RAM_Type;
      GB           : in out Gade.GB.GB_Type;
      Address      : Word;
      Value        : out Byte) is
      pragma Unreferenced (GB);
   begin
      if External_RAM.Enabled then
         Value := External_RAM.Current_Bank (Address mod 16#2000#);
      else
         Value := 0;
      end if;
   end Read;

   overriding
   procedure Write
     (External_RAM : in out External_RAM_Type;
      GB           : in out Gade.GB.GB_Type;
      Address      : Word;
      Value        : Byte) is
      pragma Unreferenced (GB);
   begin
      if External_RAM.Enabled then
         External_RAM.Current_Bank (Address mod 16#2000#) := Value;
      end if;
   end Write;

end Gade.Dev.External_RAM;
