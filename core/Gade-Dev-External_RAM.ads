package Gade.Dev.External_RAM is
   -- For the (often) battery backed RAM that some cartridge types include

   subtype External_RAM_IO_Address is Word range 16#A000#..16#BFFF#; -- 8kB

   type External_RAM_Bank_Range is range 0..3; -- Max is 32kB

   type External_RAM_Type is new Memory_Mapped_Device with private;

   procedure Reset (External_RAM : in out External_RAM_Type);

   procedure Set_Enabled
     (External_RAM : in out External_RAM_Type;
      Enabled      : Boolean);

   procedure Switch_Banks
     (External_RAM : in out External_RAM_Type;
      Bank         : External_RAM_Bank_Range);

   procedure Read
     (External_RAM : in out External_RAM_Type;
      GB           : in out Gade.GB.GB_Type;
      Address      : Word;
      Value        : out Byte);

   procedure Write
     (External_RAM : in out External_RAM_Type;
      GB           : in out Gade.GB.GB_Type;
      Address      : Word;
      Value        : Byte);

private

   subtype Bank_Address_Range is Word range 0..8*1024-1; -- Bank size is 8kB
   type RAM_Bank is array (Bank_Address_Range) of Byte;
   type RAM_Bank_Access is access RAM_Bank;

   type External_RAM_Banks is array (External_RAM_Bank_Range) of RAM_Bank_Access;

   type External_RAM_Type is new Memory_Mapped_Device with record
      Enabled      : Boolean;
      Current_Bank : RAM_Bank_Access;
   end record;

end Gade.Dev.External_RAM;
