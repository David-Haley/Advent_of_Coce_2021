with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with DJH.Execution_Time;

procedure December_12 is

   subtype Cave_Names is Unbounded_String;
   Start_Cave : constant Cave_Names := To_Unbounded_String ("start");
   End_Cave : constant Cave_Names := To_Unbounded_String ("end");

   package Connected_Lists is new Ada.Containers.Vectors (Positive, Cave_Names);
   use Connected_Lists;

   subtype Visit_Counters is Integer range -1 .. Integer'Last;

   type Caves is Record
      Is_Small : Boolean;
      Visit_Count : Visit_Counters := 0;
      Connected_List : Connected_Lists.Vector := Connected_Lists.Empty_Vector;
   end record; -- Caves

   package Cave_Stores is new
     Ada.Containers.Ordered_Maps (Cave_Names, Caves);
   use Cave_Stores;

   package Path_Sets is new Ada.Containers.Ordered_Sets (Unbounded_String);
   use Path_Sets;

   procedure Get_Input (Cave_Store : out Cave_Stores.Map) is

      -- december_12 [Input_File_Name]

      subtype Cave_Indices is Boolean;

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Cave_Name_Array : array (Cave_Indices) of Cave_Names;
      Is_Small_Cave_Array : array (Cave_Indices) of Boolean;
      Cave : Caves;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_12.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         for Cave_Index in Cave_Indices loop
            Find_Token (Text, Letter_Set, Start_At, Inside, First, Last);
               Cave_Name_Array (Cave_Index) :=
                 Unbounded_Slice (Text, First, Last);
            Is_Small_Cave_Array (Cave_Index) :=
              Is_Lower (Element (Text, First));
               Start_At := Last + 1;
         end loop; -- Cave_Index in Cave_Indices
         for Cave_Index in Cave_Indices loop
            if Contains (Cave_Store, Cave_Name_Array (Cave_Index)) then
               Append (Cave_Store (Cave_Name_Array (Cave_Index)).Connected_List,
                       Cave_Name_Array (not Cave_Index));
            else
               Clear (Cave.Connected_List);
               Append (Cave.Connected_List, Cave_Name_Array (not Cave_Index));
               Cave.Is_Small := Is_Small_Cave_Array (Cave_Index);
               Include (Cave_Store, Cave_Name_Array (Cave_Index), Cave);
            end if; -- Contains (Cave_Store, Cave_Name_Array (Cave_Index))
         end loop; -- Cave_Index in Cave_Indices
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   procedure Find_Paths (Cave : in Cave_Names;
                         Cave_Store : in out Cave_Stores.Map;
                         Path : in Unbounded_String;
                         Path_Set : in out Path_Sets.Set) is

      Path_to_Here : constant Unbounded_String := Path & "," & Cave;

   begin -- Find_Paths
      if Cave = End_Cave then
         Include (Path_Set, Path_to_Here);
      elsif not (Cave_Store (Cave).Is_Small and
                   Cave_Store (Cave).Visit_Count >= 1) then
         Cave_Store (Cave).Visit_Count := Cave_Store (Cave).Visit_Count + 1;
         for C in Iterate (Cave_Store (Cave).Connected_List) loop
            Find_Paths (Cave_Store (Cave).Connected_List (C), Cave_Store,
                        Path_to_Here, Path_Set);
         end loop; -- C in Iterate (Cave_Store (Cave).Connected_List)
         Cave_Store (Cave).Visit_Count := Cave_Store (Cave).Visit_Count - 1;
      end if; -- Cave /= End_Cave
   end Find_Paths;

   Cave_Store : Cave_Stores.Map;
   Path_Set : Path_Sets.Set;

begin -- December_12
   Get_Input (Cave_Store);
   Find_Paths (Start_Cave, Cave_Store, Null_Unbounded_String, Path_Set);
   Put_Line ("Part One Answer:" & Count_Type'Image (Length (Path_Set)));
   Clear (Path_Set);
   for C in Iterate (Cave_Store) loop
      if Cave_Store (C).Is_Small and Key (C) /= Start_Cave and
        Key (C) /= End_Cave then
         Cave_Store (C).Visit_Count := - 1;
         -- Allow an extra visit
         Find_Paths (Start_Cave, Cave_Store, Null_Unbounded_String, Path_Set);
         Cave_Store (C).Visit_Count := 0;
      end if; -- Cave_Store (C).Is_Small and Key (C) /= Start_Cave and ...
   end loop; -- C in Iterate (Cave_Store)
   Put_Line ("Part Two Answer:" & Count_Type'Image (Length (Path_Set)));
   Djh.Execution_Time.Put_CPU_Time;
end December_12;
