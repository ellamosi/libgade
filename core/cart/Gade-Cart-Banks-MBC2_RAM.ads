with Ada.Sequential_IO;

package Gade.Cart.Banks.MBC2_RAM is

   type Half_Byte is mod 2**4;

   Half_Bytes : constant := 512;

   type MBC2_RAM_Content_Range is range 0 .. Half_Bytes - 1;

   type MBC2_RAM_Content_Type is array (MBC2_RAM_Content_Range) of Half_Byte;

   type MBC2_RAM_Content_Access is access MBC2_RAM_Content_Type;

   procedure Initialize (Content : out MBC2_RAM_Content_Type);

   package MBC2_RAM_IO is new Ada.Sequential_IO (MBC2_RAM_Content_Type);

   function Load
     (File : MBC2_RAM_IO.File_Type) return MBC2_RAM_Content_Access;

   procedure Save
     (File    : MBC2_RAM_IO.File_Type;
      Content : MBC2_RAM_Content_Type);

end Gade.Cart.Banks.MBC2_RAM;
