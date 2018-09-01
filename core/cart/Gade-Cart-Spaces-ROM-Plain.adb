package body Gade.Cart.Spaces.ROM.Plain is

   function Create (ROM_Content : ROM_Content_Access) return Handler_Access is
      Handler : constant Handler_Access := new Handler_Type;
   begin
      Plain.Initialize (Handler.all, ROM_Content);
      return Handler;
   end Create;

   procedure Initialize
     (Handler     : out Handler_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access)
   is
   begin
      ROM.Handler_Type (Handler).Initialize (ROM_Content);
   end Initialize;

end Gade.Cart.Spaces.ROM.Plain;

