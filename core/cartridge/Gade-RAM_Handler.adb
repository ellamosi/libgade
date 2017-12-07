package body Gade.RAM_Handler is

   overriding
   procedure Read
     (Handler : in out RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte) is
      pragma Unreferenced (Handler, GB, Address);
   begin
      Content := 16#FF#;
   end Read;

end Gade.RAM_Handler;
