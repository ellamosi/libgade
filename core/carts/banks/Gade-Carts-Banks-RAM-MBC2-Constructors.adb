with Gade.Carts.Banks.RAM.Constructors;

package body Gade.Carts.Banks.RAM.MBC2.Constructors is

   package RAM_Bank_Constructors is new Banks.RAM.Constructors;

   function Create
     (Content : RAM_Content_NN_Access) return MBC2_RAM_Bank_NN_Access
   is
      Result : constant MBC2_RAM_Bank_NN_Access := new MBC2_RAM_Bank;
   begin
      MBC2.Constructors.Initialize (Result.all, Content);
      return Result;
   end Create;

   procedure Initialize
     (B       : out MBC2_RAM_Bank'Class;
      Content : RAM_Content_NN_Access)
   is
   begin
      RAM_Bank_Constructors.Initialize (B, Content, 0);
   end Initialize;

end Gade.Carts.Banks.RAM.MBC2.Constructors;
