with Gade.Interfaces;   use Gade.Interfaces;
with Gade.Video_Buffer; use Gade.Video_Buffer;

package Gade.Audio_Buffer is

   --  TODO: Should use M cycles
   Samples_Second  : constant := CPU_Clock_Frequency; -- Hz (TO BE 1 sample per M-Cycle)
   Samples_Frame   : constant := (Samples_Second * 100) / Frame_Frequency; -- Hz

   Extra_Samples   : constant := 2_064; -- May run for some samples too long

   Maximum_Samples : constant := Samples_Frame + Extra_Samples;

   Sample_Bit_Size : constant := 16;
   Sample_Minimum  : constant := -(2 ** (Sample_Bit_Size - 1));
   Sample_Maximum  : constant := 2 ** (Sample_Bit_Size - 1) - 1;

   type Sample is range Sample_Minimum .. Sample_Maximum;
   for Sample'Size use Sample_Bit_Size;

   type Stereo_Sample is record
      Left, Right : Sample;
   end record
     with Convention => C;

   --  TODO: Rename
   type Unconstrained_Audio_Buffer_Type is array (Natural range <>) of Stereo_Sample
     with Convention => C;

   subtype Video_Frame_Buffer_Range is Natural range 0 .. Maximum_Samples - 1;

   --  TODO: Rename, Video_Frame_Audio_Buffer_Type?
   subtype Audio_Buffer_Type is
     Unconstrained_Audio_Buffer_Type (Video_Frame_Buffer_Range);

   type Audio_Buffer_Access is access all Audio_Buffer_Type
     with Convention => C;

end Gade.Audio_Buffer;
