generic
package Gade.Carts.Banks.RAM.MBC2.Constructors is

   function Create
     (Content : RAM_Content_NN_Access) return MBC2_RAM_Bank_NN_Access;

   procedure Initialize
     (B       : out MBC2_RAM_Bank'Class;
      Content : RAM_Content_NN_Access);

end Gade.Carts.Banks.RAM.MBC2.Constructors;
