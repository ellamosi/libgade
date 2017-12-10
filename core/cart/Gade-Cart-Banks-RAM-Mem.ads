package Gade.Cart.Banks.RAM.Mem is

   type Memory_RAM_Bank_Type is new RAM_Bank_Type with record
      Content : RAM_Bank_Content_Access;
   end record;

   type Memory_RAM_Bank_Access is access Memory_RAM_Bank_Type;

   overriding
   procedure Read
     (Handler : Memory_RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : out Byte);

   overriding
   procedure Write
     (Handler : in out Memory_RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : Byte);

end Gade.Cart.Banks.RAM.Mem;
