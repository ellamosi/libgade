package Gade.Cart.Banks.RAM is

   --  Each bank is 8 kB
   subtype RAM_Bank_Address is Word range 16#0000# .. 16#1FFF#;

   Bank_Address_Mask : constant Word := 16#1FFF#;

   type RAM_Bank_Type is abstract tagged private;

   type RAM_Bank_Access is access all RAM_Bank_Type'Class;

   procedure Read
     (Handler : RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : out Byte) is abstract;

   procedure Write
     (Handler : in out RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : Byte) is abstract;

   type RAM_Bank_Content_Type is array (RAM_Bank_Address) of Byte;

   type RAM_Bank_Content_Access is access RAM_Bank_Content_Type;

   --  package RAM_Bank_IO is new Ada.Sequential_IO (RAM_Bank_Content_Type);

   procedure Initialize (Bank : out RAM_Bank_Content_Type);

--     function Load
--       (File : RAM_Bank_IO.File_Type) return RAM_Bank_Content_Access;
--
--     procedure Save
--       (File : RAM_Bank_IO.File_Type;
--        Bank : RAM_Bank_Content_Type);

private

   type RAM_Bank_Type is abstract tagged null record;

end Gade.Cart.Banks.RAM;
