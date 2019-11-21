with Gade.Carts.Mem.ROM; use Gade.Carts.Mem.ROM;
with Gade.Carts.Mem.RAM; use Gade.Carts.Mem.RAM;
private with Gade.Carts.Mixins.MBC.Constructors;
private with Gade.Carts.Banks.RTC.Constructors;

package Gade.Carts.MBC3.Constructors is

   function Create
     (ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access; -- Could be made a non acces argument, type needs to be separated
      RAM_Path    : String)
      return MBC3_Cart_NN_Access;

private

   procedure Initialize
     (C           : out MBC3_Cart'Class;
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access;
      RAM_Path    : String);

   package MBC_Constructors is new MBC_Mixin.Constructors;
   use MBC_Constructors, MBC_Constructors.RAM_Constructors;
   use RAM_Constructors.RAM_Bank_Factories;

   package Banked_RAM_Mixin renames ROM_RAM_Mixin.Banked_RAM_Mixin;
   use Banked_RAM_Mixin.Banked_RAM_Spaces;
   use Banked_RAM_Mixin.Address_Space_Banks;

   package RTC_Bank_Constructors is new Banked_RAM_Mixin.RTC_Banks.Constructors;

   subtype RTC_Bank_Range is Bank_Index range 16#08# .. 16#0C#;

   type RTC_Banks is array (RTC_Bank_Range) of Bank_Access;

   type MBC3_RAM_Bank_Factory is new Default_Bank_Factory with record
      RTC   : Clock_Access;
      Banks : RTC_Banks;
   end record;

   procedure Initialize
     (Bank_Factory : in out MBC3_RAM_Bank_Factory'Class;
      Content      : RAM_Content_Access;
      RTC          : Clock_Access);

   overriding
   function Create_Bank
     (F : in out MBC3_RAM_Bank_Factory;
      I : Bank_Index) return Bank_NN_Access;

end Gade.Carts.MBC3.Constructors;
