private with Gade.Carts.Mixins.MBC;
private with Gade.Carts.RTC;

package Gade.Carts.MBC3 is

   type MBC3_Cart is new Cart with private;

   type MBC3_Cart_Access is access MBC3_Cart;

   subtype MBC3_Cart_NN_Access is not null MBC3_Cart_Access;

   overriding
   procedure Reset (C : in out MBC3_Cart);

   overriding
   procedure Report_Cycles
     (C      : in out MBC3_Cart;
      Cycles : Positive);

   overriding
   procedure Finalize (C : in out MBC3_Cart);

private
   use Gade.Carts.RTC;

   ROM_Bank_Count : constant := 128;
   RAM_Bank_Count : constant := 16; -- (4 for MBC3, 8 for MBC30) + 8 for RTC

   --  Exact behavior of the upper RAM banks, a few assuptions were made:
   --  2#0110# <= 16#6# MBC30 Bank 6 MBC3 Bank 2
   --  2#0111# <= 16#7# MBC30 Bank 7 MBC3 Bank 3
   --  2#1000# <= 16#8# RTC S
   --  2#1001# <= 16#9# RTC M
   --  2#1010# <= 16#A# RTC H
   --  2#1011# <= 16#B# RTC DL
   --  2#1100# <= 16#C# RTC DH
   --  2#1101# <= 16#D# ???? Assumed blank (FF)
   --  2#1110# <= 16#E# ???? Assumed blank (FF)
   --  2#1111# <= 16#F# ???? Assumed blank (FF)

   ROM_Index_Mask : constant Byte := 16#7F#;
   RAM_Index_Mask : constant Byte := 16#0F#;

   Latch_Sequence : constant array (0 .. 1) of Byte := (16#00#, 16#01#);

   Initial_Latch_Value : constant Byte := 16#01#;

   package MBC_Mixin is new Gade.Carts.Mixins.MBC
     (Base_Cart => Cart,
      ROM_Banks => ROM_Bank_Count,
      RAM_Banks => RAM_Bank_Count);
   use MBC_Mixin;

   type MBC3_Cart is new MBC_Cart with record
      RTC              : Clock_Access;
      Last_Latch_Value : Byte;
   end record;

   subtype ROM_Bank_Select_Address is
     Bank_Select_Address range 16#2000# .. 16#3FFF#;

   subtype RAM_Bank_Select_Address is
     Bank_Select_Address range 16#4000# .. 16#5FFF#;

   overriding
   procedure Write_Special
     (C       : in out MBC3_Cart;
      Value   : Byte);

   overriding
   procedure Select_Bank
     (C       : in out MBC3_Cart;
      Address : Bank_Select_Address;
      Value   : Byte);

   overriding
   procedure Load_RAM_File
     (C    : in out MBC3_Cart;
      File : Ada.Streams.Stream_IO.File_Type);

   overriding
   procedure Save_RAM_File
     (C    : in out MBC3_Cart;
      File : Ada.Streams.Stream_IO.File_Type);

   procedure Select_ROM_Bank (C : in out MBC3_Cart; Value : Byte);

   procedure Select_RAM_Bank (C : in out MBC3_Cart; Value : Byte);

end Gade.Carts.MBC3;
