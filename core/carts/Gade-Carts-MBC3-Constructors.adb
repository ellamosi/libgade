with Gade.Carts.Constructors;
with Gade.Carts.RTC.Constructors;

package body Gade.Carts.MBC3.Constructors is

   function Create
     (ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access;
      RAM_Path    : String)
      return MBC3_Cart_NN_Access
   is
      Result : constant MBC3_Cart_NN_Access := new MBC3_Cart;
   begin
      Initialize (Result.all, ROM_Content, Header, RAM_Path);
      return Result;
   end Create;

   procedure Initialize
     (C           : out MBC3_Cart'Class;
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access;
      RAM_Path    : String)
   is
      use Banked_RAM_Mixin.Banked_RAM_Spaces;

      RAM_Content  : RAM_Content_Access;
      Bank_Factory : MBC3_Bank_Factories.MBC3_RAM_Bank_Factory;
      Has_Battery  : Boolean;
      Has_Timer    : Boolean;
      Savable      : Boolean;
   begin
      Has_Battery := Cart_Type_Info_For_Cart (Header.Cart_Type).Battery;
      Has_Timer := Cart_Type_Info_For_Cart (Header.Cart_Type).Timer;
      C.RTC := null;
      if Has_Timer then C.RTC := RTC.Constructors.Create; end if;
      RAM_Content := Create (Header.RAM_Size, Max_Content_Size);
      Savable := Has_Battery and (Has_Timer or RAM_Content /= null);
      Gade.Carts.Constructors.Initialize (Cart (C), RAM_Path, Savable);
      MBC3_Bank_Factories.Initialize (Bank_Factory, RAM_Content, C.RTC);
      MBC_Constructors.Initialize (C, ROM_Content, RAM_Content, Bank_Factory);
      Reset (C);
   end Initialize;

   package body MBC3_Bank_Factories is

      procedure Initialize
        (Bank_Factory : in out MBC3_RAM_Bank_Factory'Class;
         Content      : RAM_Content_Access;
         RTC          : Clock_Access)
      is
      begin
         Default_Bank_Factory (Bank_Factory).Initialize (Content);
         Bank_Factory.RTC := RTC;
         Bank_Factory.Banks := (others => null);
      end Initialize;

      overriding
      function Create_Bank
        (F : in out MBC3_RAM_Bank_Factory;
         I : Bank_Index) return Bank_NN_Access
      is
         use RTC_Bank_Constructors;

         Result       : Bank_Access;
         RTC_Register : Register;
      begin
         if I in RTC_Bank_Range and F.RTC /= null then
            --  RTC available, set up the RTC banks as such
            RTC_Register := Register'Val (I - RTC_Bank_Range'First);
            F.Banks (I) := Bank_Access (Create (F.RTC, RTC_Register));
            Result := F.Banks (I);
         elsif I in RTC_Bank_Range and F.RTC = null then
            --  No RTC available, set RTC banks as blank
            Result := Bank_Access (Blank_Banks.Singleton);
         elsif I > RTC_Bank_Range'Last then
            --  Unkown behavior for banks past RTC range, set as blank
            Result := Bank_Access (Blank_Banks.Singleton);
         else --  I < RTC_Bank_Range'First
            --  Use standard memory bank factory for banks under RTC range
            Result := Default_Bank_Factory (F).Create_Bank (I);
         end if;
         return Result;
      end Create_Bank;

   end MBC3_Bank_Factories;

end Gade.Carts.MBC3.Constructors;
