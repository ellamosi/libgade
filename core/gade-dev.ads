limited with Gade.GB;

private package Gade.Dev is

   type Hardware_Device is limited interface;

   procedure Reset
     (Device : in out Hardware_Device) is abstract;

   type Memory_Mapped_Device is abstract limited new Hardware_Device with null record;

   procedure Read
     (Device : in out Memory_Mapped_Device;
      GB     : in out Gade.GB.GB_Type;
      Addr   : Word;
      Value  : out Byte) is abstract;

   procedure Write
     (Device : in out Memory_Mapped_Device;
      GB     : in out Gade.GB.GB_Type;
      Addr   : Word;
      Value  : Byte) is abstract;

   type Memory_Access_Type is (Named, Address);

   type Interrupt_Source is limited interface;

   procedure Report_Cycles
     (Device : in out Interrupt_Source;
      GB     : in out Gade.GB.GB_Type;
      Cycles : Positive) is abstract;

end Gade.Dev;
