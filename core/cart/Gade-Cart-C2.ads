--  with Gade.Cart.Banked.ROM;
--  with Gade.Cart.Banked.RAM;

--  private
package Gade.Cart.C2 is

--     subtype External_ROM_IO_Address is Word range 16#0000# .. 16#7FFF#;
--     subtype External_RAM_IO_Address is Word range 16#A000# .. 16#BFFF#;
--
--     type Cart is abstract tagged private;
--
--     type Cart_Access is access all Cart;
--
--     --  subtype Cart_NN_Access is not null Cart_Access;
--
--     --  procedure Initialize (C : out Cart);
--
--     procedure Read_ROM
--       (C       : in out Cart;
--        Address : External_ROM_IO_Address;
--        V       : out Byte);
--
--     procedure Write_ROM
--       (C       : in out Cart;
--        Address : External_ROM_IO_Address;
--        V       : Byte) is null;
--
--     procedure Read_RAM
--       (C       : in out Cart;
--        Address : External_RAM_IO_Address;
--        V       : out Byte);
--
--     procedure Write_RAM
--       (C       : in out Cart;
--        Address : External_RAM_IO_Address;
--        V       : Byte) is null;
--
--  private
--
--     Blank_Value : constant Byte := 16#FF#;
--
--     type Cart is abstract tagged record
--        null;
--        --  Current_ROM_Banks : ROM_Banks;
--        --  Current_RAM_Bank  : Gade.Cart.Banked.RAM.Handler_Access;
--     end record;

end Gade.Cart.C2;
