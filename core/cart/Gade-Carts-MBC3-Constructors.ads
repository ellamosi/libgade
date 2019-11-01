private with Gade.Carts.Mixins.Banked.ROM.Constructors;
private with Gade.Carts.Mixins.Banked.RAM.Constructors;
--  private with Gade.Carts.Banks.RTC;
private with Gade.Carts.Banks.RTC.Constructors;
with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;
--  with Gade.Carts.Mixins.Banked.RAM.Factories;

package Gade.Carts.MBC3.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      Header   : Cart_Header_Access; -- Could be made a non acces argument, type needs to be separated
      RAM_Path : String)
      return MBC3_Cart_NN_Access;

private


   package Banked_ROM_Constructors is new Banked_ROM_Mixin.Constructors;
   package Banked_RAM_Constructors is new Banked_RAM_Mixin.Constructors;
--     use Banked_RAM_Constructors, Banked_RAM_Constructors.Bank_Pool_Constructors;

--     package RTC_Banks is new Banked_RAM_Mixin.Banked_RAM_Spaces.Address_Space_Banks.RTC;
--     package RTC_Bank_Constructors is new RTC_Banks.Constructors;
--     use RTC_Banks, RTC_Bank_Constructors;

--     use Banked_RAM_Mixin.Banked_RAM_Spaces;
--     use Banked_RAM_Mixin.Banked_RAM_Spaces.Address_Space_Banks;

   package MBC3_Bank_Factories is
      use Banked_RAM_Mixin;
      use Banked_RAM_Spaces;
      use Address_Space_Banks;
      use RTC_Banks;

      package RTC_Bank_Constructors is new RTC_Banks.Constructors;
      --  use RTC_Banks, RTC_Bank_Constructors;

      use Banked_RAM_Constructors.RAM_Bank_Factories;
--        package Banked_RAM_Bank_Factories is new Banked_RAM_Mixin.Factories;
--        use Banked_RAM_Bank_Factories;

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

   --  type RTC_Factory_Banks is array (Bank_Index range 8 .. 15) of Bank_Access;

   --  type MBC3_Variants is (ROM, ROM_RAM, ROM_TIMER, ROM_RAM_TIMER);

   --  Blank_Factory <- Not really needed?
   --  Memory_Factory <- Could be a Blank Factory too with the right consturctor logic
   --  MBC3 <- Can instantiate blanks directly, can rely on Memory Factory for
   --  the rest and do specific RTC bank instantiation
   --  Conclusion, no need for dynamic mem nor factory composition!

--     type MBC3_RAM_Bank_Factory is new Default_Bank_Factory with record
--        RTC_Banks      : RTC_Factory_Banks;
--  --        Memory_Factory : Default_Bank_Factory;
--  --        Blank_Factory  : Blank_Bank_Factory;
--        RTC            : Clock_Access;
--  --        case Variant is
--  --           when ROM | ROM_TIMER => Memory_Factory : Blank_Bank_Factory;
--  --           when others => Memory_Factory : Blank_Bank_Factory;
--  --        end case;
--     end record;

--     function Create
--       (Content : RAM_Content_Access;
--        RTC     : Clock_NN_Access)
--        return MBC3_RAM_Bank_Factory;
--
--     overriding
--     function Create_Bank
--       (F : in out MBC3_RAM_Bank_Factory;
--        I : Bank_Index) return Bank_NN_Access;

   procedure Initialize
     (C        : out MBC3_Cart'Class;
      Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String); --  This is going to need more args!

end Gade.Carts.MBC3.Constructors;
