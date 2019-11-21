with Gade.Carts.Mem.ROM; use Gade.Carts.Mem.ROM;
with Gade.Carts.Mem.RAM; use Gade.Carts.Mem.RAM;
private with Gade.Carts.Mixins.ROM_RAM.Constructors;
private with Gade.Carts.Banks.RAM.MBC2.Constructors;

package Gade.Carts.MBC2.Constructors is

   function Create
     (ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access;
      RAM_Path    : String)
      return MBC2_Cart_NN_Access;

private

   package ROM_RAM_Constructors is new ROM_RAM_Mixin.Constructors;
   use ROM_RAM_Constructors;
   use RAM_Constructors.Bank_Pool_Constructors;

   package Banked_RAM_Mixin renames ROM_RAM_Mixin.Banked_RAM_Mixin;
   use Banked_RAM_Mixin.Banked_RAM_Spaces;
   use Banked_RAM_Mixin.Address_Space_Banks;

   package MBC2_RAM_Banks is new Banked_RAM_Mixin.RAM_Banks.MBC2;
   use MBC2_RAM_Banks;

   package MBC2_RAM_Bank_Constructors is new MBC2_RAM_Banks.Constructors;

   type MBC2_RAM_Bank_Factory is new Bank_Factory with record
      Content : RAM_Content_Access;
      Bank    : MBC2_RAM_Bank_Access := null;
   end record;

   overriding
   function Create_Bank
     (F : in out MBC2_RAM_Bank_Factory;
      I : Bank_Index) return Bank_NN_Access;

   procedure Initialize
     (C           : out MBC2_Cart'Class;
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access;
      RAM_Path    : String); --  This is going to need more args!

end Gade.Carts.MBC2.Constructors;
