with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;
with NT_Console;

procedure December_13 is

   subtype Coordinates is Natural;

   type Points is record
      X, Y : Coordinates;
   end record; -- Points

   function "<" (Left, Right : Points) return Boolean is

   begin -- "<"
      return Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X);
   end "<";

   function "=" (Left, Right : Points) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Point_Stores is new
     Ada.Containers.Ordered_Sets (Points);
   use Point_Stores;

   type Axies is (X_Fold, Y_Fold);

   type Folds is record
      Axis : Axies;
      Position : Coordinates;
   end record; -- Folds

   package Fold_Stores is new Ada.Containers.Vectors (Positive, Folds);
   use Fold_Stores;

   package Screen is new NT_Console;
   use Screen;

   procedure Get_Input (Point_Store : out Point_Stores.Set;
                        Fold_Store : out Fold_Stores.Vector) is

      -- december_13 [Input_File_Name]

      Fold_Sets : constant Character_Set := To_Set ("xy=");

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Point : Points;
      Fold : Folds;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_13.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Text);
      while Length (Text) > 0 loop
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Point.X := Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Point.Y := Coordinates'Value (Slice (Text, First, Last));
         Include (Point_Store, Point);
         Get_Line (Input_File, Text);
      end loop; -- Length (Text) > 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Fold_Sets, Start_At, Inside, First, Last);
         if Element (Text, First) = 'x' then
            Fold.Axis := X_Fold;
         elsif Element (Text, First) = 'y' then
            Fold.Axis := Y_Fold;
         else
            Put_Line ("Unexpected fold typ: " & Slice (Text, First, Last));
         end if; -- Element (Text, First) = 'x'
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Fold.Position := Coordinates'Value (Slice (Text, First, Last));
         Append (Fold_Store, Fold);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   function Do_Fold (Fold : in Folds;
                     Point_Store : in Point_Stores.Set)
                     return Point_Stores.Set is

      New_Point : Points;
      New_Point_Store : Point_Stores.Set;

   begin -- Do_Fold
      for P in Iterate (Point_Store) loop
         New_point := Point_Store (P);
         case Fold.Axis is
            when X_Fold =>
               if Point_Store (P).X > Fold.Position then
                  New_point.X :=
                    Fold.Position - (Point_Store (P).X - Fold.Position);
               end if; -- Point_Store (P).X > Fold.Position
            when Y_Fold =>
               if Point_Store (P).Y > Fold.Position then
                  New_Point.Y :=
                    Fold.Position - (Point_Store (P).Y - Fold.Position);
               end if; -- Point_Store (P).Y > Fold.Position
         end case; -- Fold.Axis
         Include (New_Point_Store, New_Point);
      end loop; -- P in Iterate (Point_Store)
      return New_Point_Store;
   end Do_Fold;

   procedure Put (Point_Store : in Point_Stores.Set) is

   begin -- Put;
      for P in Iterate (Point_Store) loop
         Goto_XY (Point_Store (P).X, Point_Store (P).Y);
         Put ('#');
      end loop;
   end Put;

   Point_Store : Point_Stores.Set;
   Fold_Store : Fold_Stores.Vector;

begin -- December_13
   Get_Input (Point_Store, Fold_Store);
   Clear_Screen;
   Goto_XY (X_Pos'First, Y_Pos'Last - 2);
   Put ("Part One Answer:" &
          Count_Type'Image (Length (Do_Fold (First_Element (Fold_Store),
          Point_Store))));
   for F in Iterate (Fold_Store) loop
      Point_Store := Do_Fold (Fold_Store (F), Point_Store);
   end loop; -- F in Iterate (Fold_Store)
   Put (Point_Store);
   Goto_XY (X_Pos'First, Y_Pos'Last - 1);
   DJH.Execution_Time.Put_CPU_Time;
end December_13;
