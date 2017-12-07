with Ada.Sequential_IO;

package Gade.External.ROM is

   type ROM_Size_Type is
     (ROM_256kbit,
      ROM_512kbit,
      ROM_1Mbit,
      ROM_2Mbit,
      ROM_4Mbit,
      ROM_8Mbit,
      ROM_16Mbit,
      ROM_32Mbit,
      ROM_64Mbit,
      ROM_9Mbit,
      ROM_10Mbit,
      ROM_12Mbit);
   for ROM_Size_Type use
     (ROM_256kbit => 16#00#,
      ROM_512kbit => 16#01#,
      ROM_1Mbit   => 16#02#,
      ROM_2Mbit   => 16#03#,
      ROM_4Mbit   => 16#04#,
      ROM_8Mbit   => 16#05#,
      ROM_16Mbit  => 16#06#,
      ROM_32Mbit  => 16#07#,
      ROM_64Mbit  => 16#08#,
      ROM_9Mbit   => 16#52#,
      ROM_10Mbit  => 16#53#,
      ROM_12Mbit  => 16#54#);

   subtype ROM_Bank_Address is Word range 16#0000# .. 16#3FFF#;

   type ROM_Bank_Type is array (ROM_Bank_Address) of Byte;

   type ROM_Bank_Access is access constant ROM_Bank_Type;

   Max_ROM_Banks : constant := 512; --  8 MB for MBC5

   type ROM_Bank_Range is range 0 .. Max_ROM_Banks - 1;

   type ROM_Type is array (ROM_Bank_Range) of ROM_Bank_Access;

   type ROM_Access is access ROM_Type;

   procedure Load
     (ROM  : out ROM_Type;
      Path : String);
      --  Each bank is 16 kB

   Bank_Address_Mask : constant Word := 16#3FFF#;

   Null_ROM_Bank : aliased constant ROM_Bank_Type := (others => 16#FF#);

private

   package Bank_Reader is new Ada.Sequential_IO (ROM_Bank_Type);
   use Bank_Reader;

   ROM_Bank_Count : constant array (ROM_Size_Type) of Integer :=
     (ROM_256kbit =>   2,
      ROM_512kbit =>   4,
      ROM_1Mbit   =>   8,
      ROM_2Mbit   =>  16,
      ROM_4Mbit   =>  32,
      ROM_8Mbit   =>  64,
      ROM_16Mbit  => 128,
      ROM_32Mbit  => 256,
      ROM_64Mbit  => 512,
      ROM_9Mbit   =>  72, -- These (72, 80, 96) don't exist??
      ROM_10Mbit  =>  80,
      ROM_12Mbit  =>  96);

   procedure Load_Bank
     (File : File_Type;
      Bank : out ROM_Bank_Access);

end Gade.External.ROM;
