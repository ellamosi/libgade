with Gade.Carts.Banks.Pools.Constructors;
private with Gade.Carts.Banks.RAM.Constructors;

--  TODO: Potentially define this inside Constructors instead

generic
package Gade.Carts.Mixins.Banked.RAM.Factories is

   package Bank_Pool_Constructors is new Bank_Pools.Constructors;
   use Address_Space_Banks, Bank_Pool_Constructors;

   type Default_Bank_Factory is new Bank_Factory with private;

   procedure Initialize
     (Bank_Factory : out Default_Bank_Factory'Class;
      Content      : RAM_Content_Access);

   overriding
   function Create_Bank
     (F : in out Default_Bank_Factory;
      I : Bank_Index) return Bank_NN_Access;

private

   package RAM_Bank_Constructors is new RAM_Banks.Constructors;

   type Factory_Banks is array (Bank_Index) of Bank_Access;

   type Default_Bank_Factory is new Bank_Factory with record
      Banks   : Factory_Banks; -- Keep track of created banks
      Content : RAM_Content_Access;
      N_Banks : Bank_Count;
   end record;

end Gade.Carts.Mixins.Banked.RAM.Factories;
