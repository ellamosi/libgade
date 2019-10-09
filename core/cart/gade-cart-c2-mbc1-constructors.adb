with Gade.Cart.C2.Mixins.Banked_ROM.Constructors;
with Gade.Cart.C2.Mixins.Banked_RAM.Constructors;
with Gade.Cart.C2.Mixins.Toggled_RAM.Constructors;

package body Gade.Cart.C2.MBC1.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      RAM_Path : String)
      return MBC1_Cart_NN_Access
   is
      Result : constant MBC1_Cart_NN_Access := new MBC1_Cart;
   begin
      Initialize (Result.all, Content, RAM_Path);
      return Result;
   end Create;

   package Banked_ROM_Constructors  is new Banked_ROM_Mixin.Constructors;
   package Banked_RAM_Constructors  is new Banked_RAM_Mixin.Constructors;
   package Toggled_RAM_Constructors is new Toggled_RAM_Mixin.Constructors;

   procedure Initialize
     (C        : out MBC1_Cart'Class;
      Content  : ROM_Content_Access;
      RAM_Path : String)
   is
   begin
      Banked_ROM_Constructors.Initialize (C, Content);
      Banked_RAM_Constructors.Initialize (C, RAM_Path);
      Toggled_RAM_Constructors.Initialize (C);
   end Initialize;

end Gade.Cart.C2.MBC1.Constructors;
