generic
package Gade.Carts.Banks.RAM.Constructors is

   function Create
     (Content : RAM_Content_NN_Access;
      Offset  : Memory_Content_Offset) return RAM_Bank_NN_Access;

   procedure Initialize
     (B       : out RAM_Bank'Class;
      Content : RAM_Content_NN_Access;
      Offset  : Memory_Content_Offset);

end Gade.Carts.Banks.RAM.Constructors;
