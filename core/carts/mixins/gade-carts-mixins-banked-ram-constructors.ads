with Gade.Carts.Banks.Pools.Constructors;
with Gade.Carts.Banks.RAM.Constructors;

generic
package Gade.Carts.Mixins.Banked.RAM.Constructors is

   package RAM_Bank_Constructors is new RAM_Banks.Constructors;

   package Bank_Pool_Constructors is new Bank_Pools.Constructors;
   use Bank_Pool_Constructors;
   use Address_Space_Banks;

   function Create_Offset_Bank
     (Content : RAM_Content_Access;
      Offset  : RAM_Address)
      return Bank_NN_Access;

   package RAM_Bank_Factories is new Default_Bank_Factories
     (Address            => RAM_Address,
      Content            => RAM_Content,
      Content_Access     => RAM_Content_Access,
      Bank_Count         => Bank_Count,
      Blank_Banks        => Blank_Banks,
      Create_Offset_Bank => Create_Offset_Bank);

   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access);

   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access;
      BF      : in out Bank_Factory'Class);

end Gade.Carts.Mixins.Banked.RAM.Constructors;
