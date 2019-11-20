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

private
   use Gade.Carts.RTC;

   ROM_Bank_Count : constant := 128;
   RAM_Bank_Count : constant := 16; -- 4 For actual MBC3, 8 for MBC30 + RTC

   --  0110 <= 6 MBC30 Bank 6 MBC3 Bank 2
   --  0111 <= 7 MBC30 Bank 7 MBC3 Bank 3
   --  1000 <= 8 RTC S
   --  1001 <= 9 RTC M
   --  1010 <= A RTC H
   --  1011 <= B RTC DL
   --  1100 <= C RTC DH
   --  1101 <= D ????
   --  1110 <= E ????
   --  1111 <= F ????

   ROM_Index_Mask : constant := 16#7F#;
   RAM_Index_Mask : constant := 16#0F#;

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
