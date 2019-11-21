--  TODO: Delete, not used

generic
   type Bank_Index is range <>;
package Gade.Carts.Banks.Factories is

   type Bank_Factory is abstract tagged null record;

   function Create_Bank
     (F : in out Bank_Factory;
      I : Bank_Index) return Bank_NN_Access;

end Gade.Carts.Banks.Factories;
