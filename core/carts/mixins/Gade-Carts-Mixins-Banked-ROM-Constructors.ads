with Gade.Carts.Mem.ROM; use Gade.Carts.Mem.ROM;
private with Gade.Carts.Banks.ROM.Constructors;
private with Gade.Carts.Banks.Pools.Constructors;

generic
package Gade.Carts.Mixins.Banked.ROM.Constructors is

   procedure Initialize
     (C       : in out Banked_ROM_Cart'Class;
      Content : ROM_Content_NN_Access);

private

   package ROM_Bank_Constructors is new ROM_Banks.Constructors;

   package Bank_Pool_Constructors is new Bank_Pools.Constructors;
   use Bank_Pool_Constructors;
   use Address_Space_Banks;

   function Create_Offset_Bank
     (Content : ROM_Content_Access;
      Offset  : ROM_Address)
      return Bank_NN_Access;

   package ROM_Bank_Factories is new Default_Bank_Factories
     (Address            => ROM_Address,
      Content            => ROM_Content,
      Content_Access     => ROM_Content_Access,
      Bank_Count         => Bank_Count,
      Blank_Banks        => Blank_Banks,
      Create_Offset_Bank => Create_Offset_Bank);

end Gade.Carts.Mixins.Banked.ROM.Constructors;
