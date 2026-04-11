package body Gade.Camera is

   type Pattern_Provider is limited new Provider_Interface with null record;

   overriding
   procedure Capture_Frame (Provider : Pattern_Provider; Frame : out Bitmap);

   function Pattern_Color (X : Column_Index; Y : Row_Index) return Pixel_Value;

   Pattern_Provider_Instance : aliased Pattern_Provider;
   Default                   : constant Provider_Access :=
     Pattern_Provider_Instance'Access;

   procedure Set_Capture_Active (Provider : Provider_Access; Active : Boolean) is
      Effective_Provider : constant Provider_Access :=
        (if Provider = null then Default_Provider else Provider);
   begin
      Effective_Provider.Set_Capture_Active (Active);
   end Set_Capture_Active;

   procedure Capture_Frame (Provider : Provider_Access; Frame : out Bitmap) is
      Effective_Provider : constant Provider_Access :=
        (if Provider = null then Default_Provider else Provider);
   begin
      Effective_Provider.Capture_Frame (Frame);
   end Capture_Frame;

   overriding
   procedure Capture_Frame (Provider : Pattern_Provider; Frame : out Bitmap) is
      pragma Unreferenced (Provider);
   begin
      for Y in Frame'Range (1) loop
         for X in Frame'Range (2) loop
            Frame (Y, X) := Pattern_Color (X, Y);
         end loop;
      end loop;
   end Capture_Frame;

   function Default_Provider return Provider_Access is
   begin
      return Default;
   end Default_Provider;

   function Pattern_Color (X : Column_Index; Y : Row_Index) return Pixel_Value is
      In_Border : constant Boolean :=
        X < 8
        or else X >= Capture_Width - 8
        or else Y < 8
        or else Y >= Capture_Height - 8;
   begin
      if In_Border then
         return 3;
      else
         return Pixel_Value ((X / 32) mod 4);
      end if;
   end Pattern_Color;

end Gade.Camera;
