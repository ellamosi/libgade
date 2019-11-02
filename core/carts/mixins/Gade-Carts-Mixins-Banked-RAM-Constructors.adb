package body Gade.Carts.Mixins.Banked.RAM.Constructors is

   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access;
      Path    : String)
   is
      BF : Default_Bank_Factory;
   begin
      Initialize (BF, Content);
      Initialize (C, Content, Path, BF);
   end Initialize;

   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access;
      Path    : String;
      BF      : in out Bank_Factory'Class)
   is
   begin
      C.Content := Content;
      C.Path := new String'(Path);
      Initialize (C.Banks, BF);
      --  The following might belong to reset
      C.Accessible_Index := 0;
      C.Enabled := Enabled_Default;
      if Enabled_Default then
         C.Accessible_Bank := Select_Bank (C.Banks, C.Accessible_Index);
      else
         C.Accessible_Bank := Bank_Access (Blank_Banks.Singleton);
      end if;
   end Initialize;

end Gade.Carts.Mixins.Banked.RAM.Constructors;
