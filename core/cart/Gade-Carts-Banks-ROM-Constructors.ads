generic
package Gade.Carts.Banks.ROM.Constructors is

   function Create
     (Content : ROM_Content_NN_Access;
      Offset  : Memory_Content_Offset) return ROM_Bank_NN_Access;

   procedure Initialize
     (B       : out ROM_Bank'Class;
      Content : ROM_Content_NN_Access;
      Offset  : Memory_Content_Offset);

   function Create
     (Content : ROM_Content_NN_Access) return Partial_ROM_Bank_NN_Access;

   procedure Initialize
     (B       : out Partial_ROM_Bank'Class;
      Content : ROM_Content_NN_Access);

end Gade.Carts.Banks.ROM.Constructors;
