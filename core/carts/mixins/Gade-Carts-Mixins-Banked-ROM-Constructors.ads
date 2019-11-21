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
   package ROM_Bank_Pool_Constructors is new Bank_Pools.Constructors;
   use Address_Space_Banks, Bank_Pools;

   procedure Initialize_Banks
     (Pool    : out Bank_Pool;
      Content : ROM_Content_NN_Access);

end Gade.Carts.Mixins.Banked.ROM.Constructors;
