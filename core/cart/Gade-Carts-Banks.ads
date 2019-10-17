generic
   Size : in Word;
   type Bank_Index is range <>;
package Gade.Carts.Banks is

   subtype Bank_Address is Word range 16#0000# .. Size;

   type Bank is abstract tagged private;

   procedure Read
     (B       : in out Bank;
      Address : Bank_Address;
      V       : out Byte) is abstract;

   procedure Write (B : in out Bank; Address : Bank_Address; V : Byte) is null;

   --  This has to go away, offsets should be inmutable
   procedure Set_Bank (B : in out Bank; I : Bank_Index) is abstract;

private

   type Bank is abstract tagged null record;

end Gade.Carts.Banks;
