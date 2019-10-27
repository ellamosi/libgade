private with Gade.Carts.Banks.RAM.Constructors;
with Gade.Carts.Banks.Pools.Constructors;

generic
package Gade.Carts.Mixins.Banked.RAM.Constructors is

   package Bank_Pool_Constructors is new Bank_Pools.Constructors;
   use Address_Space_Banks, Bank_Pool_Constructors;

   --  This will eventually need config and cart info objects... or not
   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access;
      Path    : String);

   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access;
      Path    : String;
      BF      : in out Bank_Factory'Class);

private

   package RAM_Bank_Constructors is new RAM_Banks.Constructors;

--     package Bank_Factories is new Address_Space_Banks.Factories;
--     use Bank_Factories;

--     type RAM_Bank_Factory is new Bank_Factory with null record;
--
--     overriding
--     function Create_Bank (F : RAM_Bank_Factory) return Bank_NN_Access;
--
--     Banks_For_RAM_Size : constant array (RAM_Size_Type)
--       of Bank_Count :=
--         (None        =>  0,
--          RAM_16kbit  =>  1,  --   2kB
--          RAM_64kbit  =>  1,  --   8kB
--          RAM_256kbit =>  4,  --  32kB
--          RAM_512kbit =>  8,  --  64kB
--          RAM_1Mbit   => 16); -- 128kB

--     procedure Initialize_Banks
--       (Banks   : out Bank_Pool;
--        Content : RAM_Content_Access;
--        BF      : in out Bank_Factory'Class);

--     function Create_Default_Bank_Factory
--       (Content : RAM_Content_NN_Access)
--        return Bank_Factory'Class;
--
--     function Create_Blank_Bank_Factory return Bank_Factory'Class;

   type Factory_Banks is array (Bank_Index) of Bank_Access;

   type Default_Bank_Factory is new Bank_Factory with record
      Banks   : Factory_Banks; -- Keep track of created banks
      Content : RAM_Content_Access;
      N_Banks : Bank_Count;
   end record;

   overriding
   function Create_Bank
     (F : in out Default_Bank_Factory;
      I : Bank_Index) return Bank_NN_Access;

   type Blank_Bank_Factory is new Bank_Factory with null record;

   overriding
   function Create_Bank
     (F : in out Blank_Bank_Factory;
      I : Bank_Index) return Bank_NN_Access;

end Gade.Carts.Mixins.Banked.RAM.Constructors;
