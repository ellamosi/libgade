with Gade.Dev.Video.Tile_Buffer; use Gade.Dev.Video.Tile_Buffer;

package Gade.Dev.Video.Tile_Map is

   type Consolidated_Tile_Map_Type; -- is private;

   procedure Consolidate_Tile_Index
     (Tile_Map : in out Consolidated_Tile_Map_Type;
      Address  : Word;
      Value    : Byte);

   Map_Width  : constant := 32;
   Map_Height : constant := 32;
   Map_Size   : constant := Map_Width * Map_Height;

   type Horizontal_Tile_Map_Range is range 0 .. Map_Width - 1;
   type Vertical_Tile_Map_Range is range 0 .. Map_Height - 1;

   type Positional_Tile_Map_Type is array
     (Vertical_Tile_Map_Range,
      Horizontal_Tile_Map_Range) of Tile_Index_Type;

   type Addessed_Tile_Map_Type is array
     (Word range 0 .. Map_Size - 1) of Tile_Index_Type;

   type Consolidated_Tile_Map_Access_Type is (Positional, Addressed);

   type Consolidated_Partial_Map_Type
     (Access_Type : Consolidated_Tile_Map_Access_Type := Positional) is record
      case Access_Type is
         when Positional =>
            Positional : Positional_Tile_Map_Type;
         when Addressed =>
            Addressed : Addessed_Tile_Map_Type;
      end case;
   end record with Unchecked_Union;

   type Consolidated_Tile_Map_Type is array
     (Tile_Data_Access_Type) of Consolidated_Partial_Map_Type;

   procedure Reset (Tile_Map : out Consolidated_Tile_Map_Type);

private

   function Low_Data_Index (Value : Byte) return Tile_Index_Type;

   function High_Data_Index (Value : Byte) return Tile_Index_Type;

end Gade.Dev.Video.Tile_Map;
