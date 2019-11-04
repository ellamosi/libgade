private with Gade.Carts.Mixins.Banked.ROM.Constructors;
private with Gade.Carts.Mixins.Banked.RAM.Constructors;
private with Gade.Carts.Banks.RTC.Constructors;
with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

package Gade.Carts.MBC3.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      Header   : Cart_Header_Access; -- Could be made a non acces argument, type needs to be separated
      RAM_Path : String)
      return MBC3_Cart_NN_Access;

private

   package Banked_ROM_Constructors is new Banked_ROM_Mixin.Constructors;
   package Banked_RAM_Constructors is new Banked_RAM_Mixin.Constructors;

   package MBC3_Bank_Factories is
      use Banked_RAM_Mixin;
      use Banked_RAM_Spaces;
      use Address_Space_Banks;
      use RTC_Banks;

      package RTC_Bank_Constructors is new RTC_Banks.Constructors;
      use Banked_RAM_Constructors.RAM_Bank_Factories;

      type MBC3_RAM_Bank_Factory is new Default_Bank_Factory with private;

      procedure Initialize
        (Bank_Factory : in out MBC3_RAM_Bank_Factory'Class;
         Content      : RAM_Content_Access;
         RTC          : Clock_Access);

      overriding
      function Create_Bank
        (F : in out MBC3_RAM_Bank_Factory;
         I : Bank_Index) return Bank_NN_Access;

   private

      subtype RTC_Bank_Range is Bank_Index range 16#8# .. 16#C#;

      type RTC_Banks is array (RTC_Bank_Range) of Bank_Access;

      type MBC3_RAM_Bank_Factory is new Default_Bank_Factory with record
         RTC   : Clock_Access;
         Banks : RTC_Banks;
      end record;

   end MBC3_Bank_Factories;

   procedure Initialize
     (C        : out MBC3_Cart'Class;
      Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String);

end Gade.Carts.MBC3.Constructors;
