package Gade.External.RAM is

   type RAM_Size_Type is
     (None,
      RAM_16kbit,
      RAM_64kbit,
      RAM_256kbit,
      RAM_1Mbit,
      RAM_512kbit);
   for RAM_Size_Type use
     (None        => 0,
      RAM_16kbit  => 1,
      RAM_64kbit  => 2,
      RAM_256kbit => 3,
      RAM_1Mbit   => 4,
      RAM_512kbit => 5);

   subtype RAM_Bank_Address is Word range 16#0000# .. 16#1FFF#;

   type RAM_Bank_Type is array (RAM_Bank_Address) of Byte;

   type RAM_Bank_Access is access RAM_Bank_Type;

   Max_RAM_Banks : constant := 16; --  128 kB for MBC5

   type RAM_Bank_Range is range 0 .. Max_RAM_Banks - 1;

   type RAM_Type is array (RAM_Bank_Range) of RAM_Bank_Access;

   type RAM_Access is access RAM_Type;

private

   RAM_Bank_Count : constant array (RAM_Size_Type) of Integer :=
     (None        =>  0,
      RAM_16kbit  =>  1,
      RAM_64kbit  =>  1,
      RAM_256kbit =>  4,
      RAM_1Mbit   => 16,
      RAM_512kbit =>  8);

end Gade.External.RAM;
