private with Gade.Carts.Mixins.Banked.ROM.Constructors;
private with Gade.Carts.Mixins.Banked.RAM.Constructors;
private with Gade.Carts.Banks.RAM.MBC2.Constructors;
with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

package Gade.Carts.MBC2.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      Header   : Cart_Header_Access; -- Could be made a non acces argument, type needs to be separated
      RAM_Path : String)
      return MBC2_Cart_NN_Access;

private

   package Banked_ROM_Constructors is new Banked_ROM_Mixin.Constructors;

   package MBC2_RAM_Constructors is new MBC2_RAM_Mixin.Constructors;
   --  use MBC2_RAM_Constructors, MBC2_RAM_Constructors.Bank_Pool_Constructors;

   --  package MBC2_RAM_Banks is new MBC2_RAM_Mixin.RAM_Banks.MBC2;
   package MBC2_RAM_Bank_Constructors is new MBC2_RAM_Banks.Constructors;
   use MBC2_RAM_Constructors.RAM_Bank_Factories.Bank_Pool_Constructors; -- Not pretty
   use MBC2_RAM_Banks;
   use MBC2_RAM_Mixin.Banked_RAM_Spaces;
   use MBC2_RAM_Mixin.Banked_RAM_Spaces.Address_Space_Banks;

   type MBC2_RAM_Bank_Factory is new Bank_Factory with record
      Content : RAM_Content_Access;
      Bank    : MBC2_RAM_Bank_Access := null;
   end record;

   overriding
   function Create_Bank
     (F : in out MBC2_RAM_Bank_Factory;
      I : Bank_Index) return Bank_NN_Access;

   procedure Initialize
     (C        : out MBC2_Cart'Class;
      Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String); --  This is going to need more args!

end Gade.Carts.MBC2.Constructors;
