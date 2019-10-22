generic
package Gade.Carts.Mixins.Banked_RAM.Constructors is

   procedure Initialize
     (C    : in out Banked_RAM_Cart'Class;
      Size : RAM_Size_Type;
      Path : String); --  This needs config and cart info objects

private

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
