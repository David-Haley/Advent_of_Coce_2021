with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_15 is

   subtype Coordinates is Natural;
   subtype Extended_Coordinated is Integer range -1 .. Coordinates'Last;
   subtype Risk_Levels is Positive range 1 .. 9;

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

   type Risk_Data is record
      Risk_Level : Risk_Levels;
      In_Path : Boolean := False;
   end record; -- Risk_Data

   package Point_Stores is new
     Ada.Containers.Ordered_Maps (Points, Risk_Data);
   use Point_Stores;

   procedure Get_Input (Point_Store : out Point_Stores.Map) is

      -- december_15 [Input_File_Name]

      Input_File : File_Type;
      Text : Unbounded_String;
      Point : Points;
      Risk_Datum : Risk_Data;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_15.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Point.Y := 0;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for I in Positive range 1 .. Length (Text) loop
            Point.X := I - 1;
            Risk_Datum.Risk_Level :=Risk_Levels'Value (Slice (Text, I, I));
            Include (Point_Store, Point, Risk_Datum);
         end loop;
         Point.Y := Point.Y + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   function Minimum_Risk (Point_Store_in : in Point_Stores.map)
                          return Natural is

      type Queue_Elements is record
         Point : Points;
         Accumulated_Risk : Natural;
      end record; -- Queue_Elements

      function Get_Accumulated_Risk (Queue_Element : Queue_Elements)
                                     return Natural is

      begin -- Get_Accumulated_Risk
         return Queue_Element.Accumulated_Risk;
      end Get_Accumulated_Risk;

      function Risk_Before (Left, Right : Natural) return Boolean is

      begin -- Risk_Before
         return Left < Right;
      end Risk_Before;

      package Queue_Interfaces is new
        Ada.Containers.Synchronized_Queue_Interfaces (Queue_Elements);

      package Queues is new
        Ada.Containers.Unbounded_Priority_Queues
          (Queue_Interfaces => Queue_Interfaces,
           Queue_Priority => Natural,
           Get_Priority => Get_Accumulated_Risk,
           Before => Risk_Before);
      use Queues;

      Point_Queue : Queues.Queue;

      function Valid_Point (X, Y : in Extended_Coordinated;
                            Point_Store : in Point_Stores.Map) return Boolean is

      begin -- Valid_Point
         return (X >= Coordinates'First and Y >= Coordinates'First) and then
           Contains (Point_Store, (X, Y)) and then
           not Point_Store ((X, Y)).In_Path;
      end Valid_Point;

      procedure Enqueue (X, Y : in Coordinates;
                         Queue_Element : in Queue_Elements;
                         Point_Store : in out Point_Stores.Map) is

         New_Queue_Element : Queue_Elements;

      begin -- Enqueue
         New_Queue_Element.Point := (X, Y);
         New_Queue_Element.Accumulated_Risk := Queue_Element.Accumulated_Risk +
           Point_Store (New_Queue_Element.Point).Risk_Level;
         Point_Store (New_Queue_Element.Point).In_Path := True;
         Point_Queue.Enqueue (New_Queue_Element);
      end Enqueue;

      Point_Store : Point_Stores.Map := Copy (Point_Store_In);
      X, Y :  Extended_Coordinated;
      Queue_Element : Queue_Elements;
      Found : Boolean := False;

   begin -- Minimum_Risk
      Queue_Element.Point := First_Key (Point_Store);
      Queue_Element.Accumulated_Risk := 0;
      Point_Store (Queue_Element.Point).In_Path := True;
      Point_Queue.Enqueue (Queue_Element);
      while Point_Queue.Current_Use > 0 and not Found loop
         Point_Queue.Dequeue (Queue_Element);
         if Queue_Element.Point = Last_Key (Point_Store) then
            -- destination found
            Found := True;
         else
            -- stop if risk is already greater than a previously found path
            X := Queue_Element.Point.X;
            Y := Queue_Element.Point.Y + 1; -- down
            if Valid_Point (X, Y, Point_Store) then
               Enqueue (X, Y,Queue_Element, Point_Store);
            end if; -- Valid_Point (X, Y, Point_Store)
            X := Queue_Element.Point.X + 1; -- right
            Y := Queue_Element.Point.Y;
            if Valid_Point (X, Y, Point_Store) then
               Enqueue (X, Y,Queue_Element, Point_Store);
            end if; -- Valid_Point (X, Y, Point_Store)
            X := Queue_Element.Point.X - 1; -- left
            Y := Queue_Element.Point.Y;
            if Valid_Point (X, Y, Point_Store) then
               Enqueue (X, Y,Queue_Element, Point_Store);
            end if; -- Valid_Point (X, Y, Point_Store)
            X := Queue_Element.Point.X;
            Y := Queue_Element.Point.Y - 1; -- up
            if Valid_Point (X, Y, Point_Store) then
               Enqueue (X, Y,Queue_Element, Point_Store);
            end if; -- Valid_Point (X, Y, Point_Store)
         end if; -- Queue_Element.Point = Last_Key (Point_Store)
      end loop; -- Point_Queue.Current_Use > 0 and not Found
      if Found then
         return Queue_Element.Accumulated_Risk;
      else
         return Natural'Last;
      end if; -- Found
   end Minimum_Risk;

   Procedure Make_Big_Map (Small_Map : in Point_Stores.Map;
                           Big_Map : out Point_Stores.Map) is

      X_Multiplier : constant Coordinates := Last_Key (Small_Map).X + 1;
      Y_Multiplier : constant Coordinates := Last_Key (Small_Map).X + 1;
      Small_Map_Point, Big_Map_Point : Points;
      Big_Map_Datum : Risk_Data;

   begin -- Make_Big_Map
      for P in Iterate (Small_Map) loop
         for Y_Modifier in Coordinates range 0 .. 4 loop
            for X_Modifier in Coordinates range 0 .. 4 loop
               Small_Map_Point := Key (P);
               Big_Map_Point.X := Small_Map_Point.X + X_Modifier * X_Multiplier;
               Big_Map_Point.Y := Small_Map_Point.Y + Y_Modifier * Y_Multiplier;
               Big_Map_Datum.Risk_Level :=
                 (Coordinates (Small_Map (P).Risk_Level) + X_Modifier +
                    Y_Modifier - 1) mod 9 + 1;
               Include (Big_Map, Big_Map_Point, Big_Map_Datum);
            end loop; -- X_Modifier in Coordinates range 0 .. 4
         end loop; -- Y_Modifier in Coordinates range 0 .. 4
      end loop; -- P in Iterate (Point_Stores)
   end Make_Big_Map;

   Point_Store, Big_Map : Point_Stores.Map;

begin -- December_15
   Get_Input (Point_Store);
   Put_Line ("Part One Answer:" & Natural'Image (Minimum_Risk (Point_Store)));
   Make_Big_Map (Point_Store, Big_Map);
   Put_Line ("Part Two Answer:" & Natural'Image (Minimum_Risk (Big_Map)));
   DJH.Execution_Time.Put_CPU_Time;
end December_15;
