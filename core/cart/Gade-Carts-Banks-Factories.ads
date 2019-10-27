--  with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

generic
package Gade.Carts.Banks.Factories is

   type Bank_Factory is abstract tagged null record;

   function Create_Bank (F : Bank_Factory) return Bank_NN_Access;

end Gade.Carts.Banks.Factories;
