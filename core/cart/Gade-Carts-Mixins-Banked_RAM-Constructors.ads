private with Gade.Carts.Banks.RAM;
private with Gade.Carts.Banks.RAM.Constructors;
private with Gade.Carts.Banks.Blank;
private with Gade.Carts.Banks.Pools.Constructors;

generic
package Gade.Carts.Mixins.Banked_RAM.Constructors is

   procedure Initialize
     (C    : in out Banked_RAM_Cart'Class;
      Size : RAM_Size_Type;
      Path : String); --  This needs config and cart info objects

private

   package RAM_Banks is new RAM_Space_Banks.RAM;
   package RAM_Bank_Constructors is new RAM_Banks.Constructors;
   package Blank_Banks is new RAM_Space_Banks.Blank;
   package Bank_Pool_Constructors is new RAM_Bank_Pools.Constructors;

   Banks_For_RAM_Size : constant array (RAM_Size_Type)
     of Bank_Count :=
       (None        =>  0,
        RAM_16kbit  =>  1,  --   2kB
        RAM_64kbit  =>  1,  --   8kB
        RAM_256kbit =>  4,  --  32kB
        RAM_512kbit =>  8,  --  64kB
        RAM_1Mbit   => 16); -- 128kB

   procedure Initialize_Banks
     (Banks   : out Bank_Pool;
      Content : RAM_Content_Access;
      Size    : RAM_Size_Type);

end Gade.Carts.Mixins.Banked_RAM.Constructors;
