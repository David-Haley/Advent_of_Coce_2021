with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_14 is

   Part_One_Limit : constant Positive := 10;
   Part_Two_Limit : constant Positive := 40;

   subtype Compounds is Unbounded_String;

   subtype Reagent_Pairs is String (1 .. 2);

   subtype Reagents is Character with
     Static_Predicate => Reagents in 'A' .. 'Z';

   package Formular_Stores is new
     Ada.Containers.Ordered_Maps (Reagent_Pairs, Reagents);
   use Formular_Stores;

   subtype Pair_Counts is Unsigned_64;

   procedure Get_Input (Template : out Compounds;
                        Formular_Store : out Formular_Stores.Map) is

      -- december_14 [Input_File_Name]

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Reagent_Pair: Reagent_Pairs;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_14.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Template);
      Skip_Line (Input_File);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Upper_Set, Start_At, Inside, First, Last);
         Reagent_Pair := Slice (Text, First, Last);
         Start_At := Last + 1;
         Find_Token (Text, Upper_Set, Start_At, Inside, First, Last);
         Include (Formular_Store, Reagent_Pair, Element (Text, First));
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   function Solve (Template : in Compounds;
                   Formular_Store : in Formular_Stores.Map;
                   Steps : in Positive) return Pair_Counts is

      -- Much like the day six the secret is to look at the population in this
      -- case letter pairs rather than calculating an actual outcome, that is,
      -- the resulting "polymer". With the rate of growth two things happen
      -- with a brute force calculation firstly the execution increases in a
      -- power law proportional 2 ** Steps. Also the storage required increases
      -- at the same rate!

      package Pair_Count_Stores is new
        Ada.Containers.Ordered_Maps (Reagent_Pairs, Pair_Counts);
      use  Pair_Count_Stores;

      package Histograms is new
        Ada.Containers.Ordered_Maps (Reagents, Pair_Counts);
      use Histograms;

      Pair_Count_Store, New_Pair_Count_Store : Pair_Count_Stores.Map;
      New_Reagent : Reagents;
      New_Reagent_Pair : Reagent_Pairs;
      Histogram : Histograms.Map;
      Highest_Frequency : Pair_Counts := Pair_Counts'First;
      Lowest_Frequency : Pair_Counts := Pair_Counts'Last;

   begin -- Solve
      -- nnitialise the data structures;
      for F in Iterate (Formular_Store) loop
         Include (Pair_Count_Store, Key (F), 0);
         Include (New_Pair_Count_Store, Key (F), 0);
         Include (Histogram, Formular_Store (F), 0);
      end loop; -- F in Iterate (Formular_Store)
      -- The two outer letters remain unchanged and are only single counted, so
      -- the histogram has to be initialised for these special cases.
      Histogram (Element (Template, 1)) := 1;
      Histogram (Element (Template, Length (Template))) := 1;
      for E in Positive range 1 .. Length (Template) - 1 loop
         New_Reagent_Pair := Slice (Template, E, E + 1);
         Pair_Count_Store (New_Reagent_Pair) :=
           Pair_Count_Store (New_Reagent_Pair) + 1;
      end loop; -- E in Positive range 1 .. Length (Template) - 1
      for I in Positive range 1 .. Steps loop
         For N in Iterate (New_Pair_Count_Store) loop
            New_Pair_Count_Store (N) := 0;
         end loop; -- N in Iterate (New_Pair_Count_Store)
         for P in Iterate (Pair_Count_Store) loop
            New_Reagent := Formular_Store (Key (P));
            New_Reagent_Pair  (1) :=  Key (P) (1);
            New_Reagent_Pair (2) := New_Reagent;
            New_Pair_Count_Store (New_Reagent_Pair) :=
              New_Pair_Count_Store (New_Reagent_Pair) + Pair_Count_Store (P);
            New_Reagent_Pair (1) := New_Reagent;
            New_Reagent_Pair (2) := Key (P) (2);
            New_Pair_Count_Store (New_Reagent_Pair) :=
              New_Pair_Count_Store (New_Reagent_Pair) + Pair_Count_Store (P);
         end loop; -- P in Iterate (Pair_Count_Store)
         Pair_Count_Store := New_Pair_Count_Store;
      end loop; -- I in Positive range 1 .. Steps
      for P in Iterate (New_Pair_Count_Store) loop
         -- because we increment for both letters this causes a double count for
         -- all the "inside" Reagent_Pairs
         Histogram (Key (P) (1)) := Histogram (Key (P) (1)) +
           New_Pair_Count_Store (P);
         Histogram (Key (P) (2)) := Histogram (Key (P) (2)) +
           New_Pair_Count_Store (P);
      end loop; -- P in Iterate (New_Pair_Count_Store)
      for R in Iterate (Histogram) loop
         if Histogram (R) > Highest_Frequency then
            Highest_Frequency := Histogram (R);
         end if; -- Histogram (R) > Highest_Frequency
         if Lowest_Frequency > Histogram (R) then
            Lowest_Frequency := Histogram (R);
         end if; -- Lowest_Frequency > Histogram
      end loop; -- R in Iterate (Histogram)
      return (Highest_Frequency - Lowest_Frequency) / 2;
      -- Dividing by 2 is required because we count each letter in the pair thus
      -- double counting letters.
   end Solve;

   Template : Compounds;
   Formular_Store : Formular_Stores.Map;

begin -- December_14
   Get_Input (Template, Formular_Store);
   Put_Line ("Part_One Answer:" & Solve (Template, Formular_Store, 10)'Img);
   Put_Line ("Part_Two Answer:" & Solve (Template, Formular_Store, 40)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_14;
