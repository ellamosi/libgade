package Gade.Cart.ROM is

   Max_ROM_Banks : constant := 512; --  8 MB for MBC5
   Max_ROM_Bytes : constant := 512 * 8 * 1024; -- 8 MB for MBC5 (512 x 8kByte)

   type ROM_Bank_Count is range 0 .. Max_ROM_Banks;
   type ROM_Byte_Count is range 0 .. Max_ROM_Bytes;

   subtype ROM_Bank_Range is ROM_Bank_Count range 0 .. Max_ROM_Banks - 1;
   subtype ROM_Address_Range is ROM_Byte_Count range 0 .. Max_ROM_Bytes - 1;

   type ROM_Content_Type is array (ROM_Address_Range range <>) of aliased Byte;
   type ROM_Content_Access is access ROM_Content_Type;

   function Load (Path : String) return ROM_Content_Access;

   --  TODO: Possibly move header access somewhere else
   subtype ROM_Header_Range is ROM_Address_Range range 16#100# .. 16#14F#;
   subtype ROM_Header_Content_Type is ROM_Content_Type (ROM_Header_Range);
   type ROM_Header_Content_Access is access all ROM_Header_Content_Type;

--     type Header_Union_Access_Type is (Header, Bytes);
--     type Header_Union is record
--       (Tile_Array_Access  : Header_Union_Access_Type := Header) is record
--        case Tile_Array_Access is
--           when Header =>
--              Header : Cart_Header_Type;
--           when Bytes =>
--              Low_Tile_Data    : Low_Raster_Tile_Array_Type;
--        end case;
--     end record with Unchecked_Union;

--     type ROM_Wrapper_Type (Addresses : ROM_Address_Range) is record
--        Header : Cart_Header;
--     end record;
--     for ROM_Wrapper_Type use record
--        Header at 16#100# range 0 .. 8 * 16#50#;
--     end record;
--     --for ROM_Wrapper_Type'Size use 8 * (16#9800# - 16#8000#);
--     type ROM_Wrapper_Access is access ROM_Wrapper_Type;

   function Header (ROM : ROM_Content_Access) return Cart_Header_Access;

--
--     function Convert is new Ada.Unchecked_Conversion
--       (Source => ROM_Content_Access,
--        Target => ROM_Wrapper_Access);

end Gade.Cart.ROM;
