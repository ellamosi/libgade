package body Gade.Cart.RAM.Handlers.MBC2 is

   function Create
     (Path : String) return MBC2_RAM_Handler_Access
   is
      Handler : constant MBC2_RAM_Handler_Access := new MBC2_RAM_Handler_Type;
   begin
      MBC2.Initialize (Handler.all, Path);
      return Handler;
   end Create;

   procedure Initialize
     (Handler : out MBC2_RAM_Handler_Type'Class;
      Path    : String)
   is
   begin
      Handler.RAM_Content := new RAM_Content_Type (Content_Range);
      Handler.Path := new String'(Path);
      Load (Handler.Path.all, Handler.RAM_Content.all);
      Handler.Memory_Bank := new Memory_RAM_Bank_Type;
      Handler.Reset;
   end Initialize;

   overriding procedure Reset
     (Handler : in out MBC2_RAM_Handler_Type)
   is
   begin
      Handler.Enabled := False;
   end Reset;

   overriding procedure Read
     (Handler : in out MBC2_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

   overriding procedure Write
     (Handler : in out MBC2_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

   overriding procedure Set_Enabled
     (Handler : in out MBC2_RAM_Handler_Type;
      Enabled : Boolean)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Set_Enabled unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Enabled";
   end Set_Enabled;

   overriding procedure Toggle_Enabled
     (Handler : in out MBC2_RAM_Handler_Type)
   is
   begin
      Handler.Enabled := not Handler.Enabled;
   end Toggle_Enabled;

   overriding procedure Save
     (Handler : MBC2_RAM_Handler_Type)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Save unimplemented");
      raise Program_Error with "Unimplemented procedure Save";
   end Save;

end Gade.Cart.RAM.Handlers.MBC2;
