package Gade.GB.Memory_Map is

   procedure Read_Byte
     (GB      : in out GB_Type;
      Address : Word;
      Value   : out Byte);

   -- TODO: Re-examine function with mutable orguments
   function Read_Byte
     (GB      : in out GB_Type;
      Address : Word) return Byte;

   procedure Write_Byte
     (GB      : in out GB_Type;
      Address : Word;
      Value   : Byte);

   procedure Read_Word
     (GB      : in out GB_Type;
      Address : Word;
      Value   : out Word);

   -- TODO: Re-examine function with mutable orguments
   function Read_Word
     (GB      : in out GB_Type;
      Address : Word) return Word;

   procedure Write_Word
     (GB      : in out GB_Type;
      Address : Word;
      Value   : Word);

   procedure Push
     (GB    : in out GB_Type;
      Value : Word);

   procedure Pop
     (GB : in out GB_Type;
      Value : out Word);

end Gade.GB.Memory_Map;
