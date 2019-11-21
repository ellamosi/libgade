generic
package Gade.Carts.Banks.RAM.Constructors is

   function Create
     (Content : RAM_Content_NN_Access;
      Offset  : RAM_Address) return RAM_Bank_NN_Access;

   procedure Initialize
     (B       : out RAM_Bank'Class;
      Content : RAM_Content_NN_Access;
      Offset  : RAM_Address);

end Gade.Carts.Banks.RAM.Constructors;
