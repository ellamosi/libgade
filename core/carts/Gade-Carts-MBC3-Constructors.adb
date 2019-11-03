package body Gade.Carts.MBC3.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String)
      return MBC3_Cart_NN_Access
   is
      Result : constant MBC3_Cart_NN_Access := new MBC3_Cart;
   begin
      Initialize (Result.all, Content, Header, RAM_Path);
      return Result;
   end Create;

   procedure Initialize
     (C        : out MBC3_Cart'Class;
      Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String)
   is
      use Banked_RAM_Mixin.Banked_RAM_Spaces;

      RAM_Content  : RAM_Content_Access;
      Bank_Factory : MBC3_Bank_Factories.MBC3_RAM_Bank_Factory;
   begin
      C.RTC := null;
      --  TODO: Define constant for this
      if Header.Cart_Type in ROM_MBC3_TIMER_BATT .. ROM_MBC3_TIMER_RAM_BATT then
         C.RTC := new Clock; -- TODO: use proper constructor
         Reset (C.RTC.all);
      end if;
      RAM_Content := Create (Header.RAM_Size, Max_Content_Size);
      MBC3_Bank_Factories.Initialize (Bank_Factory, RAM_Content, C.RTC);
      Banked_ROM_Constructors.Initialize (C, Content);
      Banked_RAM_Constructors.Initialize (C, RAM_Content, RAM_Path, Bank_Factory);
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
         Result   : Bank_Access;
         Register : Clock_Register;
      begin
         if I in RTC_Bank_Range and F.RTC /= null and F.RTC /= null then
            --  RTC available, set up the RTC banks as such
            --  TODO: figure out which RTC register call the constructor with
            Register := Clock_Register'Val (I - RTC_Bank_Range'First);
            F.Banks (I) := Bank_Access (RTC_Bank_Constructors.Create (F.RTC, Register));
            Result := F.Banks (I);
         elsif I in RTC_Bank_Range and F.RTC /= null and F.RTC = null then
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
