package body Gade.Carts.Banks is

   package body Memory_Bank_Mixin is

      procedure Initialize
        (B       : out Memory_Bank'Class;
         Content : Content_NN_Access;
         Offset  : Address)
      is
      begin
         B.Content := Content_Access (Content);
         B.Offset := Offset;
         B.Address_Mask := Address_Mask (Content.all);
      end Initialize;

      overriding
      procedure Read
        (B    : in out Memory_Bank;
         Addr : Bank_Address;
         V    : out Byte)
      is
      begin
         V := B.Content (Decode (B, Addr));
      end Read;

      function Decode
        (B    : Memory_Bank'Class;
         Addr : Bank_Address)
         return Address
      is
      begin
         return Address (Addr and B.Address_Mask) + B.Offset;
      end Decode;

      function Address_Mask (Mem : Content) return Bank_Address is
         --  Assumes that Content_Size is already a power of 2. The content
         --  should  have been padded to satisfy that in case the ROM file had
         --  been trimmed.
         --
         --  RAM bank mask example:
         --  Size: 16#2000# => 2#10_0000_0000_0000#
         --  Mask: 16#1FFF# => 2#01_1111_1111_1111#
         Bank_Size : constant Positive := Size;
         Mem_Size  : constant Positive := Mem'Length;

         Accessible_Size : Positive;
      begin
         if Mem_Size <= Bank_Size then
            Accessible_Size := Mem_Size;
         else
            --  A bit of a silly edge case, but a ROM could potentially be of a
            --  bigger size than addressable while still reporting no bank
            --  controller. In this case ignore everything past the bank size.
            Accessible_Size := Bank_Size;
         end if;
         return Bank_Address (Accessible_Size - 1);
      end Address_Mask;

   end Memory_Bank_Mixin;

end Gade.Carts.Banks;
