with Gade.Input_Reader; use Gade.Input_Reader;

package Gade.Dev.Joypad is

   subtype Joypad_IO_Address is Word range 16#FF00#..16#FF00#;

   type Joypad_Type is
     new Memory_Mapped_Device and Interrupt_Source with private;

   overriding procedure Reset (Device : in out Joypad_Type);

   overriding procedure Read
     (Joypad  : in out Joypad_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte);

   overriding procedure Write
     (Joypad  : in out Joypad_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte);

   overriding procedure Report_Cycle
     (Joypad : in out Joypad_Type;
      GB     : in out Gade.GB.GB_Type);

   procedure Set_Input_Reader
     (Joypad : in out Joypad_Type;
      Reader : Input_Reader_Access);

private

   type Joypad_Matrix_Access_Type is (Named, Byte_Access);
   type Joypad_Matrix_Type
     (Access_Type : Joypad_Matrix_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            P10_IN  : Boolean;
            P11_IN  : Boolean;
            P12_IN  : Boolean;
            P13_IN  : Boolean;
            P14_OUT : Boolean;
            P15_OUT : Boolean;
         when Byte_Access =>
            Reg : Byte;
      end case;
   end record;
   pragma Unchecked_Union (Joypad_Matrix_Type);
   for Joypad_Matrix_Type use record
      P10_IN  at 0 range 0..0;
      P11_IN  at 0 range 1..1;
      P12_IN  at 0 range 2..2;
      P13_IN  at 0 range 3..3;
      P14_OUT at 0 range 4..4;
      P15_OUT at 0 range 5..5;
   end record;
   for Joypad_Matrix_Type'Size use 8;

   type Joypad_Type is
     new Memory_Mapped_Device and Interrupt_Source with record
      Reader : Input_Reader_Access := null;
      Map    : Joypad_Matrix_Type;
   end record;

end Gade.Dev.Joypad;
