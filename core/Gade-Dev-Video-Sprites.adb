with Gade.Dev.VRAM; use Gade.Dev.VRAM;

package body Gade.Dev.Video.Sprites is

   type Prioritized_Sprites is array (1 .. Max_Line_Sprites) of Sprite_Index_Type;

   type Sprite_Priority_Buffer is record
      Indexes   : Prioritized_Sprites;
      N_Sprites : Natural := 0;
   end record;

   procedure Insert
     (Buffer   : in out Sprite_Priority_Buffer;
      Sprites  : Sprite_Array_Type;
      Index    : Sprite_Index_Type;
      Inserted : out Boolean);
   procedure Insert
     (Buffer   : in out Sprite_Priority_Buffer;
      Sprites  : Sprite_Array_Type;
      Index    : Sprite_Index_Type;
      Inserted : out Boolean)
   is
      --  1 : More Prio .. 10 : Less Prio
      function Compare_Priorities (L, R : Sprite_Index_Type) return Boolean;
      procedure Sort_Buffer;
      function Greater_Than_Last return Boolean;

      function Compare_Priorities (L, R : Sprite_Index_Type) return Boolean is
      begin
         return Sprites (L).X < Sprites (R).X or (Sprites (L).X = Sprites (R).X and L < R);
      end Compare_Priorities;

      procedure Sort_Buffer is
         procedure Swap (L, R : in out Sprite_Index_Type);
         procedure Swap (L, R : in out Sprite_Index_Type) is
            Temp : Sprite_Index_Type;
         begin
            Temp := L;
            L := R;
            R := Temp;
         end Swap;

         i : Natural := Buffer.N_Sprites - 1;
      begin
         while i >= 1 and then Compare_Priorities (Buffer.Indexes (i), Buffer.Indexes (i + 1)) loop
            Swap (Buffer.Indexes (i), Buffer.Indexes (i + 1));
            i := i - 1;
         end loop;
      end Sort_Buffer;

      function Greater_Than_Last return Boolean is
      begin
         return Compare_Priorities (Index, Buffer.Indexes (Buffer.N_Sprites));
      end Greater_Than_Last;

      Buffer_Full : constant Boolean := Buffer.N_Sprites = Buffer.Indexes'Last;
   begin
      if Buffer_Full and then Greater_Than_Last then
         Buffer.Indexes (Buffer.N_Sprites) := Index;
         Sort_Buffer;
         Inserted := True;
      elsif not Buffer_Full then
         Buffer.N_Sprites := Buffer.N_Sprites + 1;
         Buffer.Indexes (Buffer.N_Sprites) := Index;
         Sort_Buffer;
         Inserted := True;
      else
         Inserted := False;
      end if;
   end Insert;

   procedure Populate_Line_Cache
     (VRAM  : Gade.Dev.VRAM.VRAM_Type;
      OAM   : Gade.Dev.OAM.OAM_Type;
      Cache : out Sprite_Line_Cache;
      Row   : Display_Vertical_Range;
      Size  : Sprite_Size_Type)
   is
      Sprite_X, Sprite_Y : Natural; -- Bottom right corner of the sprite (+1)
      Sprite_Row : Integer;
      X_Flip, Y_Flip : Boolean;
      Col : Integer;
      Tile_Index : Tile_Index_Type;
      Tile_Row, Tile_Col : Natural;
      Color : Color_Value;

      Y_Limit : constant Positive := Sprite_Height (Size);
      Index_Add : Tile_Index_Type;

      Inserted : Boolean := True;
      Priority_Buffer : Sprite_Priority_Buffer;

      function Is_Enabled (Sprite : Sprite_Type) return Boolean;
      function Is_Enabled (Sprite : Sprite_Type) return Boolean is
         Sprite_Row : Integer;
      begin
         Sprite_Row := Row - (Natural (Sprite.Y) - 16);
         return Sprite_Row in 0 .. Y_Limit - 1;
      end Is_Enabled;
   begin
      --  Reset cache
      for Element of Cache loop
         Element.Value := Sprite_Transparent_Color;
      end loop;

      --  Prepare priority buffer
      --  TODO: 10 Sprites drawn only dictated by index order ONLY
      --  Color priority then uses X_Pos among these
      for Sprite_Index in OAM.Map.Sprites'Range loop
         if Is_Enabled (OAM.Map.Sprites (Sprite_Index)) then
            Insert (Priority_Buffer, OAM.Map.Sprites, Sprite_Index, Inserted);
            if not Inserted then exit; end if;
         end if;
      end loop;

      for Current_Sprite of Priority_Buffer.Indexes (1 .. Priority_Buffer.N_Sprites) loop
         Sprite_X := Natural (OAM.Map.Sprites (Current_Sprite).X);
         Sprite_Y := Natural (OAM.Map.Sprites (Current_Sprite).Y);
         Sprite_Row := Row - (Sprite_Y - Double_Sprite_Height);

         if Sprite_X in 1 .. 160 + 8 - 1 and Sprite_Row in Sprite_Vertical_Range then
            --  Sprite is visible

            X_Flip := OAM.Map.Sprites (Current_Sprite).X_Flip;
            Y_Flip := OAM.Map.Sprites (Current_Sprite).Y_Flip;

            Index_Add := Sprite_Index_Add_Lookup (Size, Y_Flip, Sprite_Row);
            Tile_Row := Y_Flip_Lookup (Size, Y_Flip, Sprite_Row);
            Tile_Index := Tile_Index_Type (OAM.Map.Sprites (Current_Sprite).Pattern) + Index_Add;

            for Sprite_Col in Sprite_Horizontal_Range loop
               Col := Sprite_X - 8 + Sprite_Col;

               if Col in Display_Horizontal_Range then
                  Tile_Col := X_Flip_Lookup (X_Flip, Col - Sprite_X + 8);
                  Color := Read_Raster_Tile (VRAM.Tile_Buffer, Tile_Index, Tile_Row, Tile_Col);

                  if Color /= Sprite_Transparent_Color then
                     Cache (Col).Value := Color;
                     --  TODO: Needs a better name or enum!
                     Cache (Col).Priority := OAM.Map.Sprites (Current_Sprite).Priority;
                     Cache (Col).Palette := OAM.Map.Sprites (Current_Sprite).Palette;
                  end if;
               end if;
            end loop;
         end if;
      end loop;
   end Populate_Line_Cache;

end Gade.Dev.Video.Sprites;
