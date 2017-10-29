limited with Gade.GB;

package Gade.Dev.Interrupts is

   subtype Interrupt_Flag_IO_Address is Word range 16#FF0F# .. 16#FF0F#;

   subtype Interrupt_Enable_IO_Address is Word range 16#FFFF# .. 16#FFFF#;

   type Interrupt_Flag_Type is
     new Memory_Mapped_Device with private;

   overriding procedure Reset (Interrupt_Flag : in out Interrupt_Flag_Type);

   overriding procedure Read
     (Interrupt_Flag : in out Interrupt_Flag_Type;
      GB             : in out Gade.GB.GB_Type;
      Address        : Word;
      Value          : out Byte);

   overriding procedure Write
     (Interrupt_Flag : in out Interrupt_Flag_Type;
      GB             : in out Gade.GB.GB_Type;
      Address        : Word;
      Value          : Byte);

   type Interrupt_Enable_Type is
     new Memory_Mapped_Device with private;

   overriding procedure Reset (Interrupt_Enable : in out Interrupt_Enable_Type);

   overriding procedure Read
     (Interrupt_Enable : in out Interrupt_Enable_Type;
      GB               : in out Gade.GB.GB_Type;
      Address          : Word;
      Value            : out Byte);

   overriding procedure Write
     (Interrupt_Enable : in out Interrupt_Enable_Type;
      GB               : in out Gade.GB.GB_Type;
      Address          : Word;
      Value            : Byte);

   type Interrupt_Type is
     (VBlank_Interrupt,
      LCDC_Interrupt,
      Timer_Interrupt,
      Serial_Interrupt,
      Joypad_Interrupt);

   procedure Set_Interrupt
     (GB        : in out Gade.GB.GB_Type;
      Interrupt : Interrupt_Type);

   procedure Service_Interrupts
     (GB     : in out Gade.GB.GB_Type;
      Cycles : out Natural);

private

   Interrupt_Handlers : constant array (Interrupt_Type) of Word :=
     (16#0040#, 16#0048#, 16#0050#, 16#0058#, 16#0060#);

   type Interrupt_Flag_Access_Type is (Named, Address, Indexed);
   type Indexed_Flag_Array is array (Interrupt_Type) of Boolean;
   pragma Pack (Indexed_Flag_Array);

   type Interrupt_Flag_Register_Type
     (Access_Type : Interrupt_Flag_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            VBLANK : Boolean;
            LCDC   : Boolean;
            Timer  : Boolean;
            Serial : Boolean;
            Joypad : Boolean;
         when Address =>
            Reg : Byte;
         when Indexed =>
            Flags : Indexed_Flag_Array;
      end case;
   end record;
   pragma Unchecked_Union (Interrupt_Flag_Register_Type);
   for Interrupt_Flag_Register_Type use record
      VBLANK at 0 range 0 .. 0;
      LCDC   at 0 range 1 .. 1;
      Timer  at 0 range 2 .. 2;
      Serial at 0 range 3 .. 3;
      Joypad at 0 range 4 .. 4;
      Reg    at 0 range 0 .. 7;
   end record;
   for Interrupt_Flag_Register_Type'Size use 8;

   type Interrupt_Flag_Type is new Memory_Mapped_Device with record
      Map : Interrupt_Flag_Register_Type;
   end record;

--  ----------------------------------------------+---------------+---------------
--  FFFF -- ISWITCH [RW] Interrupt Enable/Disable | when set to 1 | when set to 0
--  Bit4  Transition High->Low on pins P10-P13    | ENABLED       | DISABLED
--  Bit3  End of serial I/O transfer              | ENABLED       | DISABLED
--  Bit2  Timer overflow                          | ENABLED       | DISABLED
--  Bit1  LCD controller interrupt [see LCDSTAT]  | ENABLED       | DISABLED
--  Bit0  LCD vertical blanking impulse           | ENABLED       | DISABLED
   type Interrupt_Enable_Register_Type
     (Access_Type : Interrupt_Flag_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            VBLANK : Boolean;
            LCDC   : Boolean;
            Timer  : Boolean;
            Serial : Boolean;
            Joypad : Boolean;
         when Address =>
            Reg : Byte;
         when Indexed =>
            Flags : Indexed_Flag_Array;
      end case;
   end record;
   pragma Unchecked_Union (Interrupt_Enable_Register_Type);
   for Interrupt_Enable_Register_Type use record
      VBLANK at 0 range 0 .. 0;
      LCDC   at 0 range 1 .. 1;
      Timer  at 0 range 2 .. 2;
      Serial at 0 range 3 .. 3;
      Joypad at 0 range 4 .. 4;
      Reg    at 0 range 0 .. 7;
   end record;
   for Interrupt_Enable_Register_Type'Size use 8;

   Interrupt_Enable_Mask : constant := 2#00011111#;

   type Interrupt_Enable_Type is new Memory_Mapped_Device with record
      Map : Interrupt_Flag_Register_Type;
   end record;

   function Interrupt_Requested
     (Interrupt_Flag : Interrupt_Flag_Register_Type) return Boolean;

end Gade.Dev.Interrupts;
