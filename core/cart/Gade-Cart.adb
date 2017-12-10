with Ada.Text_IO; use Ada.Text_IO;

with Gade.Cart.ROM;                   use Gade.Cart.ROM;
with Gade.Cart.ROM.Handlers;          use Gade.Cart.ROM.Handlers;
with Gade.Cart.ROM.Handlers.ROM_Only; use Gade.Cart.ROM.Handlers.ROM_Only;
with Gade.Cart.ROM.Handlers.MBC.MBC1; use Gade.Cart.ROM.Handlers.MBC.MBC1;

with Gade.Cart.RAM.Handlers;          use Gade.Cart.RAM.Handlers;
with Gade.Cart.RAM.Handlers.Blank;    use Gade.Cart.RAM.Handlers.Blank;
with Gade.Cart.RAM.Handlers.Banked;   use Gade.Cart.RAM.Handlers.Banked;

with Gade.Cart.Banks.ROM;             use Gade.Cart.Banks.ROM;

package body Gade.Cart is
   package ROM_Only_Handler renames Gade.Cart.ROM.Handlers.ROM_Only;
   package MBC1_ROM_Handler renames Gade.Cart.ROM.Handlers.MBC.MBC1;

   package Blank_RAM_Handler  renames Gade.Cart.RAM.Handlers.Blank;
   package Banked_RAM_Handler renames Gade.Cart.RAM.Handlers.Banked;

   procedure Load_ROM
     (ROM_Handler : out ROM_Handler_Access;
      RAM_Handler : out RAM_Handler_Access;
      Path        : String)
   is
      ROM_Content : ROM_Content_Access;
      Header : Cart_Header_Access;

      Controller : Controller_Type;
      RAM_Handler_Kind : RAM_Handler_Kind_Type;
   begin
      ROM_Content := Load (Path);
      Header := Convert (ROM_Content (0)); --  Not true for some few rare cart types
      Controller := Controller_Type_For_Cart (Header.Cart_Type);
      RAM_Handler_Kind := RAM_Handler_Kind_For_Cart (Header.Cart_Type);

      Put_Line ("Cartridge type: " & Header.Cart_Type'Img);
      Put_Line ("Controller type: " & Controller'Img);

      RAM_Handler :=
        (case RAM_Handler_Kind is
            when None => RAM_Handler_Access (Blank_RAM_Handler.Create),
            when MBC1 => RAM_Handler_Access (Banked_RAM_Handler.Create (Header.RAM_Size)));

      ROM_Handler :=
        (case Controller is
            when Cartridge_Info.None =>
               ROM_Handler_Access (ROM_Only_Handler.Create (ROM_Content)),
            when Cartridge_Info.MBC1 =>
               ROM_Handler_Access (MBC1_ROM_Handler.Create (ROM_Content, RAM_Handler)),
            when others =>
               raise Program_Error with "Unsupported cartridge controller! " & Controller'Img);


   end Load_ROM;

end Gade.Cart;
