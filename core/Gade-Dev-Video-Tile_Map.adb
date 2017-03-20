package body Gade.Dev.Video.Tile_Map is

   function Low_Data_Index (Value : Byte) return Tile_Index_Type is
   begin
      return Tile_Index_Type(Value);
   end Low_Data_Index;

   function High_Data_Index (Value : Byte) return Tile_Index_Type is
   begin
      return Tile_Index_Type(256 + Integer(To_Signed(Value)));
   end High_Data_Index;

   procedure Consolidate_Tile_Index
     (Tile_Map : in out Consolidated_Tile_Map_Type;
      Address  : Word;
      Value    : Byte)
   is
      Map_Index : Word;
   begin
      Map_Index := Address mod Map_Size;
      Tile_Map(Low_Data).Addressed(Map_Index) := Low_Data_Index(Value);
      Tile_Map(High_Data).Addressed(Map_Index) := High_Data_Index(Value);
   end Consolidate_Tile_Index;

end Gade.Dev.Video.Tile_Map;
