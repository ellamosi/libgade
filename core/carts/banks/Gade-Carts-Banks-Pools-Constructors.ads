with Gade.Carts.Banks.Blank;

generic
package Gade.Carts.Banks.Pools.Constructors is

   type Bank_Factory is abstract tagged null record;

   function Create_Bank
     (F           : in out Bank_Factory;
      I           : Bank_Index;
      Deallocable : Boolean) return Bank_NN_Access is abstract;

   generic
      type Address is range <>;
      type Content is array (Address range <>) of Byte;
      type Content_Access is access Content;
      type Bank_Count is range <>;
      with package Blank_Banks is new Gade.Carts.Banks.Blank (<>);

      with function Create_Offset_Bank
        (Content : Content_Access;
         Offset  : Address)
         return Bank_NN_Access;
   package Default_Bank_Factories is

      type Default_Bank_Factory is new Bank_Factory with private;

      procedure Initialize
        (Bank_Factory : out Default_Bank_Factory'Class;
         Content      : Content_Access);

      overriding
      function Create_Bank
        (F           : in out Default_Bank_Factory;
         I           : Bank_Index;
         Deallocable : Boolean) return Bank_NN_Access;

   private

      type Factory_Banks is array (Bank_Index) of Bank_Access;

      type Default_Bank_Factory is new Bank_Factory with record
         Banks   : Factory_Banks; -- Keep track of created banks
         Content : Content_Access;
         N_Banks : Bank_Count;
      end record;

   end Default_Bank_Factories;

   procedure Initialize
     (Pool : out Bank_Pool;
      BF   : in out Bank_Factory'Class);

end Gade.Carts.Banks.Pools.Constructors;
