with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure December_05 is

   subtype Coordinates is Natural;

   type Points is record
      X, Y : Coordinates;
   end record; -- Points

   function "<" (Left, Right : Points) return Boolean is

   begin -- "<"
      return Left.X < Right.X or (Left.X = Right.X and Left.Y < Right.Y);
   end "<";

   function "=" (Left, Right : Points) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Point_Stores is new
     Ada.Containers.Ordered_Maps (Points, Positive);
   use Point_Stores;

   type Lines is record
      P1, P2 : Points;
   end record; -- Lines

   package Line_Stores is new Ada.Containers.Vectors (Positive, Lines);
   use Line_Stores;

   procedure Increment_Point (Point_Store : in out Point_Stores.Map;
                              Point : in Points) is

   begin -- Increment_Point
      if Contains (Point_Store, Point) then
         Point_Store (Point) := Point_Store (Point) + 1;
      else
         Insert (Point_Store, Point, 1);
      end if; -- Contains (Point_Store, Point)
   end Increment_Point;

   function Count_Points (Point_Store : in Point_Stores.Map) return Natural is

      Point_Count : Natural := 0;

   begin -- Count_Points
      for P in Iterate (Point_Store) loop
         if Point_Store (P) > 1 then
            Point_Count := Point_Count + 1;
         end if; -- Point_Store (P) > 1
      end loop; -- P in Iterate (Point_Store)
      return Point_Count;
   end Count_Points;

   Input_File : File_Type;
   Text : Unbounded_String;
   Line_Store : Line_Stores.Vector;
   Point_Store : Point_Stores.Map;
   Start_At, First : Positive;
   Last : Natural;
   Line : Lines;
   Point : Points;
   dX, dY : Integer;

begin -- December_05
   if Argument_Count = 0 then
      Open (Input_File, In_File, "december_05.txt");
   else
      Open (Input_File, In_File, Argument(1));
   end if; -- Argument_Count = 0
   while not End_Of_File (Input_File) loop
      Get_Line (Input_File, Text);
      Start_At := 1;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Line.P1.X := Coordinates'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Line.P1.Y := Coordinates'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Line.P2.X := Coordinates'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Line.P2.Y := Coordinates'Value (Slice (Text, First, Last));
      Append (Line_Store, Line);
   end loop; -- not End_Of_File (Input_File)
   Close (Input_File);
   for L in Iterate (Line_Store) loop
      if Line_Store (L).P1.X = Line_Store (L).P2.X then
         -- Vertical Line
         Point := Line_Store (L).P1;
         if Line_Store (L).P2.Y > Line_Store (L).P1.Y then
            dY := 1;
         else
            dY := -1;
         end if; -- Line_Store (L).P2.Y > Line_Store (L).P1.Y
         loop -- step through Y range
            Increment_Point (Point_Store, Point);
            exit when Point.Y = Line_Store (L).P2.Y;
            Point.Y := Point.Y + dY;
         end loop; -- step through Y range
      elsif Line_Store (L).P1.Y = Line_Store (L).P2.Y then
         -- Horizontal Line
         Point := Line_Store (L).P1;
         if Line_Store (L).P2.X > Line_Store (L).P1.X then
            dX := 1;
         else
            dX := -1;
         end if; -- Line_Store (L).P2.X > Line_Store (L).P1.X
         loop -- step through X range
            Increment_Point (Point_Store, Point);
            exit when Point.X = Line_Store (L).P2.X;
            Point.X := Point.X + dX;
         end loop; -- step through X range
      end if; -- Line_Store (L).P1.X = Line_Store (L).P2.X
   end loop; -- L in Iterate (Line_Store)
   Put_Line ("Part One Answer:" & Natural'Image (Count_Points (Point_Store)));
   for L in Iterate (Line_Store) loop
      if Line_Store (L).P1.X /= Line_Store (L).P2.X and
        Line_Store (L).P1.Y /= Line_Store (L).P2.Y then
         -- Note only lines with gradient -1 or 1 need to be supported!
         Point := Line_Store (L).P1;
         if Line_Store (L).P2.X > Line_Store (L).P1.X then
            dX := 1;
         else
            dX := -1;
         end if; -- Line_Store (L).P2.X > Line_Store (L).P1.X
         if Line_Store (L).P2.Y > Line_Store (L).P1.Y then
            dY := 1;
         else
            dY := -1;
         end if; -- Line_Store (L).P2.Y > Line_Store (L).P1.Y
         loop -- step through both X and Y range
            Increment_Point (Point_Store, Point);
            exit when Point.X = Line_Store (L).P2.X or
              Point.Y = Line_Store (L).P2.Y;
            Point.X := Point.X + dX;
            Point.Y := Point.Y + dY;
         end loop; -- step through both X and Y range
      end if; -- Line_Store (L).P1.X /= Line_Store (L).P2.X and ...
   end loop; -- L in Iterate (Line_Store)
   Put_Line ("Part Two Answer:" & Natural'Image (Count_Points (Point_Store)));
end December_05;
