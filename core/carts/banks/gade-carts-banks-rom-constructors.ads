generic
package Gade.Carts.Banks.ROM.Constructors is

   function Create
     (Content : ROM_Content_NN_Access;
      Offset  : ROM_Address) return ROM_Bank_NN_Access;

   procedure Initialize
     (B       : out ROM_Bank'Class;
      Content : ROM_Content_NN_Access;
      Offset  : ROM_Address);

end Gade.Carts.Banks.ROM.Constructors;
