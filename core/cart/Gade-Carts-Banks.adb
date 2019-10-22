package body Gade.Carts.Banks is

   package body Memory_Bank_Mixin is

      procedure Initialize_Base
        (B       : out Base_Memory_Bank'Class;
         Content : Content_NN_Access)
      is
      begin
         B.Content := Content_Access (Content);
      end Initialize_Base;

      overriding procedure Read
        (B : in out Memory_Bank; Address : Bank_Address; V : out Byte)
      is
      begin
         V := B.Content (Address + B.Offset);
      end Read;

      procedure Initialize_Full
        (B       : out Memory_Bank'Class;
         Content : Content_NN_Access;
         Offset  : Memory_Content_Offset)
      is
      begin
         Base_Memory_Bank (B).Initialize_Base (Content);
         B.Offset := Offset;
      end Initialize_Full;

      overriding
      procedure Read
        (B       : in out Partial_Memory_Bank;
         Address : Bank_Address;
         V       : out Byte)
      is
      begin
         V := B.Content (Memory_Content_Address (Address and B.Address_Mask));
      end Read;

      procedure Initialize_Partial
        (B            : out Partial_Memory_Bank'Class;
         Content      : Content_NN_Access)
      is
      begin
         Base_Memory_Bank (B).Initialize_Base (Content);
         B.Address_Mask := Address_Mask (Content.all'Length);
      end Initialize_Partial;

   end Memory_Bank_Mixin;

   function "+" (Left : Bank_Address; Right : Memory_Content_Offset)
                 return Memory_Content_Address
   is
   begin
      return Memory_Content_Address (Left) + Right;
   end "+";

   function Address_Mask (Content_Size : Natural) return Bank_Address
   is
      --  Assumes that Content_Size is already a power of 2. The content should
      --  have been padded to satisfy that in case the ROM file had been
      --  trimmed.
      --
      --  RAM bank mask example:
      --  Size: 16#2000# => 2#10_0000_0000_0000#
      --  Mask: 16#1FFF# => 2#01_1111_1111_1111#
      Accessible_Size : Content_Byte_Count;
   begin
      if Content_Byte_Count (Content_Size) <= Content_Byte_Count (Size) then
         Accessible_Size := Content_Byte_Count (Content_Size);
      else
         --  A bit of a silly edge case, but a ROM could potentially be of a
         --  bigger size than addressable while still reporting no bank
         --  controller.
         Accessible_Size := Content_Byte_Count (Size);
      end if;
      return Bank_Address (Accessible_Size - 1);
   end Address_Mask;

end Gade.Carts.Banks;
