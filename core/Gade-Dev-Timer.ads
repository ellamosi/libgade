package Gade.Dev.Timer is

   subtype Timer_IO_Address is Word range 16#FF04#..16#FF07#;

   DIV  : constant Word := 16#FF04#;
   TIMA : constant Word := 16#FF05#;
   TMA  : constant Word := 16#FF06#;
   TAC  : constant Word := 16#FF07#;

   type Timer_Type is
     new Memory_Mapped_Device and Interrupt_Source with private;

   procedure Reset
     (Timer : in out Timer_Type);

   overriding procedure Read
     (Timer   : in out Timer_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte);

   overriding procedure Write
     (Timer   : in out Timer_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : Byte);

   overriding procedure Report_Cycle
     (Timer : in out Timer_Type;
      GB    : in out Gade.GB.GB_Type);

   --  Name     - TIMA
   --  Contents - Timer counter (R/W)
   --             This timer is incremented by a clock frequency specified by
   --  the TAC register ($FF07). The timer generates an interrupt when it
   --  overflows.


   -- Name     - TMA
   -- Contents - Timer Modulo (R/W)
   --            When the TIMA overflows, this data will be loaded.


   --  Name     - TAC
   --  Contents - Timer Control (R/W)
   --             Bit 2 - Timer Stop
   --                     0: Stop Timer
   --                     1: Start Timer
   --             Bits 1+0 - Input Clock Select
   --                     00: 4. 096 KHz   ( ~4. 194 KHz SGB)
   --                     01: 262. 144 Khz ( ~268. 4 KHz SGB)
   --                     10: 65. 536 KHz  ( ~67. 11 KHz SGB)
   --                     11: 16. 384 KHz  ( ~16. 78 KHz SGB)


private

   type Timer_Address_Space is Array (Timer_IO_Address'Range) of Byte;

   -- CPU Clock: 4.194304 Mhz

   type Input_Clock_Type is (f_4_096, f_262_144, f_65_536, f_16_384);
   type Timer_Stop_Type is (Stop, Start);

   -- 1024 Clocks, 16 Clocks, 64 Clocks, 256 Clocks
   TIMA_Clocks : constant array(Input_Clock_Type) of Natural :=
     (f_4_096   => 1024,
      f_262_144 => 16,
      f_65_536  => 64,
      f_16_384  => 256);

   type Timer_Control_Type is record
      Input_Clock_Select : Input_Clock_Type;
      Timer_Stop         : Timer_Stop_Type;
   end record;
   for Timer_Control_Type use record
      Input_Clock_Select at 0 range 0..1;
      Timer_Stop         at 0 range 2..2;
   end record;
   for Timer_Control_Type'Size use 8;

   type Timer_Access_Type is (Named, Address);

   type Timer_Map_Type (Access_Type : Timer_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            Divider       : Byte;
            Timer_Counter : Byte;
            Timer_Modulo  : Byte;
            Timer_Control : Timer_Control_Type;
         when Address =>
            Space : Timer_Address_Space;
      end case;
   end record;
   pragma Unchecked_Union (Timer_Map_Type);

   type Timer_Type is
     new Memory_Mapped_Device and Interrupt_Source with record
      Ticks        : Natural;
      Modulo_Ticks : Natural;
      Map          : Timer_Map_Type;
   end record;

end Gade.Dev.Timer;
