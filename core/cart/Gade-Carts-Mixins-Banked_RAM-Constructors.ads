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
        RAM_16kbit  =>  1,
        RAM_64kbit  =>  1,
        RAM_256kbit =>  4,
        RAM_512kbit =>  8,
        RAM_1Mbit   => 16);

   Address_Mask_For_RAM_Size : constant array (RAM_Size_Type)
     of Word :=
       (None        => 16#0000#, -- Does not really matter
        RAM_16kbit  => 16#07FF#,
        RAM_64kbit  => 16#1FFF#,
        RAM_256kbit => 16#1FFF#,
        RAM_512kbit => 16#1FFF#,
        RAM_1Mbit   => 16#1FFF#);

   Content_Size_For_RAM_Size : constant array (RAM_Size_Type)
     of Content_Byte_Count :=
       (None        =>          0,
        RAM_16kbit  =>   2 * 1024,
        RAM_64kbit  =>   8 * 1024,
        RAM_256kbit =>  32 * 1024,
        RAM_512kbit =>  64 * 1024,
        RAM_1Mbit   => 128 * 1024);

   procedure Initialize_Banks
     (Banks   : out RAM_Bank_Set;
      Content : RAM_Content_Access;
      N_Banks : Bank_Count);

   procedure Initialize_Open_Banks
     (Banks   : in out RAM_Bank_Set;
      N_Banks : Bank_Count);

end Gade.Carts.Mixins.Banked_RAM.Constructors;
