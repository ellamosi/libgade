package Gade.GB.Memory_Map is

   function CPU_Bus_Blocked (GB : GB_Type; Address : Word) return Boolean;

   procedure Read_Byte (GB : in out GB_Type; Address : Word; Value : out Byte);

   --  TODO: Re-examine function with mutable orguments
   function Read_Byte (GB : in out GB_Type; Address : Word) return Byte;

   procedure CPU_Read_Byte (GB : in out GB_Type; Address : Word; Value : out Byte);

   function CPU_Read_Byte (GB : in out GB_Type; Address : Word) return Byte;

   procedure Write_Byte (GB : in out GB_Type; Address : Word; Value : Byte);

   procedure CPU_Write_Byte (GB : in out GB_Type; Address : Word; Value : Byte);

   procedure Read_Word (GB : in out GB_Type; Address : Word; Value : out Word);

   --  TODO: Re-examine function with mutable orguments
   function Read_Word (GB : in out GB_Type; Address : Word) return Word;

   procedure Write_Word (GB : in out GB_Type; Address : Word; Value : Word);

end Gade.GB.Memory_Map;
