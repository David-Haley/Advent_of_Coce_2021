with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_23 is

   type Contents is (Wall, Empty, A, B, C, D);

   subtype Amphipods is Contents range A .. D;

   type X_Coordinates is (Hall_1, Hall_2, Room_A, Hall_3, Room_B, Hall_4,
                          Room_C, Hall_5, Room_D, Hall_6, Hall_7);

   type Y_Coordinates is (Hall, Room_1, Room_2);

   type States is array (X_Coordinates, Y_Coordinates) of Contents;

   subtype Costs is Natural;

   procedure Get_Input (Initial_State : out States) is

      T_Room_A : constant Positive := 4;
      T_Room_B : constant Positive := 6;
      T_Room_C : constant Positive := 8;
      T_Room_D : constant Positive := 10;

      Input_File : File_Type;
      Text : Unbounded_String;

   begin -- Get_Input
      Initial_State := (others => (Hall => Empty, others => Wall));
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_23.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Text);
      Assert ("#############" = To_String (Text), "Top wall not found");
      Get_Line (Input_File, Text);
      Assert ("#...........#" = To_String (Text), "Hallway not found");
      for Y in Y_Coordinates range Room_1 .. Room_2 loop
         Get_Line (Input_File, Text);
         if Y = Room_1 then
            Assert ("###" = Slice (Text, 1, 3), "Start Room_1 not found");
            Assert ("###" = Slice (Text, 11, 13), "End Room_1 not found");
         else
            Assert ("  #" = Slice (Text, 1, 3), "Start Room_2 not found");
            Assert ('#' = Element (Text, 11), "End Room_2 not found");
         end if; -- Y = Room_1
         Initial_State (Room_A, Y) :=
           Amphipods'Value (Slice (Text, T_Room_A, T_Room_A));
         Initial_State (Room_B, Y) :=
           Amphipods'Value (Slice (Text, T_Room_B, T_Room_B));
         Initial_State (Room_C, Y) :=
           Amphipods'Value (Slice (Text, T_Room_C, T_Room_C));
         Initial_State (Room_D, Y) :=
           Amphipods'Value (Slice (Text, T_Room_D, T_Room_D));
      end loop; -- Y in Y_Coordinates range Room_1 .. Room_2
      Get_Line (Input_File, Text);
      Assert ("  #########" = To_String (Text), "Bottom wall not found");
   end Get_Input;

   procedure Put (State : in States) is

   begin -- Put
      for Y in Y_Coordinates loop
         for X in X_Coordinates loop
            case State (X, Y) is
               when Wall => Put ('#');
               when A | B | C | D => Put (Amphipods'Image (State (X, Y)));
               when Empty => Put (' ');
            end case; -- State (X, Y)
         end loop; -- X in X_Coordinates
         New_Line;
      end loop; -- Y in Y_Coordinates
      New_Line;
   end Put;

   procedure Solve (State : in States;
                    Cost : in Costs;
                    Least_Cost : in out Costs) is

         Room_Table : constant array (Amphipods) of X_Coordinates :=
           (A => Room_A, B => Room_B, C => Room_C, D => Room_D);

      function Solved (State : in States) return Boolean is

      begin -- Solved
         return State (Room_A, Room_1) = A and State (Room_A, Room_2) = A and
           State (Room_B, Room_1) = B and State (Room_B, Room_2) = B and
           State (Room_C, Room_1) = C and State (Room_C, Room_2) = C and
           State (Room_D, Room_1) = D and State (Room_D, Room_2) = D;
      end Solved;

      function Move_Cost (Source_X : in X_Coordinates;
                          Source_Y : in Y_Coordinates;
                          Destination_X : in X_Coordinates;
                          Destination_Y : in Y_Coordinates) return Costs is

         Cost_Table : constant array (Amphipods) of Costs :=
           (A => 1, B => 10, C => 100, D => 1000);

      begin -- Move_Cost
         return (abs (X_Coordinates'Pos (Source_X) -
                   X_Coordinates'Pos (Destination_X)) +
                 abs (Y_Coordinates'Pos (Source_Y) -
                     Y_Coordinates'Pos (Destination_Y))) *
           Cost_Table (State (Source_X, Source_Y));
      end Move_Cost;

      function Hall_Clear (X_Src_In : in X_Coordinates;
                           X_Dst : in X_Coordinates;
                           From_Hall : in Boolean := False) return Boolean is

         Result : Boolean := True;
         X_Src : X_Coordinates;

      begin -- Hall_Clear
         if X_Src_In < X_Dst then
            if From_Hall then
               X_Src := X_Coordinates'Succ (X_Src_In);
            end if; -- From_Hall;
            for X in X_Coordinates range X_Src .. X_Dst loop
               Result := Result and State (X, Hall) = Empty;
            end loop; -- X in X_Coordinates range X_Src .. X_Dst
         else
            if From_Hall then
               X_Src := X_Coordinates'Pred (X_Src_In);
            end if; -- From_Hall;
            for X in X_Coordinates range X_Dst .. X_Src loop
               Result := Result and State (X, Hall) = Empty;
            end loop; -- X in X_Coordinates range X_Dst .. X_Src
         end if; -- X_Src < X_Dst
         return Result;
      end Hall_Clear;

      Source_Y : Y_Coordinates;
      Destination_X : X_Coordinates;
      Destination_Y : Y_Coordinates;
      Next_State : States;

   begin -- Solve
      if Solved (State) then
         if Cost < Least_Cost then
            Least_Cost := Cost;
         end if; -- Cost < Least_Cost
      elsif Cost < Least_Cost then
         -- the above is huresuic pruning, if the search costs more than a
         -- solution that has already been found, don't proceed
         -- Explore moves from hallway to rooms
         Source_Y := Hall;
         for Source_X in X_Coordinates loop
            if State (Source_X, Hall) in Amphipods then
               Destination_Y := Hall; -- not a valid destination
               Destination_X := Room_Table (State (Source_X, Hall));
               if Hall_Clear (Source_X, Destination_X, True) then
                  if State (Destination_X, Room_2) = Empty then
                     Destination_Y := Room_2;
                  elsif State (Destination_X, Room_2) = State (Source_X, Hall)
                    and State (Destination_X, Room_1) = Empty then
                     Destination_Y := Room_1;
                  end if; -- State (Destination_X, Room_2) = Empty
               end if; -- Hall_Clear (Source_X, Destination_X, True)
               if Destination_Y /= Hall then
                  -- Valid destination has been set, a move is possible
                  Next_State := State;
                  Next_State (Destination_X, Destination_Y) :=
                    State (Source_X, Source_Y);
                  Next_State (Source_X, Source_Y) := Empty;
                  Solve (Next_State,
                         Cost + Move_Cost (Source_X, Source_Y,
                           Destination_X, Destination_Y),
                         Least_Cost);
               end if; -- Destination_Y /= Hall
            end if; -- State (Source_X, Hall) in Amphipods
         end loop; -- Source_X in X_Coordinates
         -- Explore moves from rooms to hall
         Destination_Y := Hall;
         for Destination_X in X_Coordinates loop
            if Destination_X /= Room_A and Destination_X /= Room_B and
              Destination_X /= Room_C and Destination_X /= Room_D and
              State (Destination_X, Destination_Y) = Empty then
               for Source_X in X_Coordinates range Room_A .. Room_D loop
                  Source_Y := Hall; -- not a valid source
                  if Hall_Clear (Source_X, Destination_X) then
                     if State (Source_X, Room_1) in Amphipods and then
                       (Source_X /= Room_Table (State (Source_X, Room_1)) or
                          Source_X /= Room_Table (State (Source_X, Room_2))) then
                        Source_Y := Room_1;
                     elsif State (Source_X, Room_2) in Amphipods and then
                       Source_X /= Room_Table (State (Source_X, Room_2)) then
                        Source_Y := Room_2;
                     end if; -- State (Source_X, Room_1) in Amphipods and then ...
                     if Source_Y /= Hall then
                        -- Valid source has been set, a move is possible
                        Next_State := State;
                        Next_State (Destination_X, Destination_Y) :=
                          Next_State (Source_X, Source_Y);
                        Next_State (Source_X, Source_Y) := Empty;
                        Solve (Next_State,
                               Cost + Move_Cost (Source_X, Source_Y,
                                 Destination_X, Destination_Y),
                               Least_Cost);
                     end if; -- Source_Y /= Hall
                  end if; -- Hall_Clear (Source_X, Destination_X)
               end loop; -- Source_X in X_Coordinates range Room_A .. Room_D
            end if; -- Destination_X /= Room_A and Destination_X /= Room_B and ...
         end loop; -- Destination_X in X_Coordinates
      end if; -- Solved (State)
   end Solve;

   Initial_State : States;
   Cost, Least_Cost : Costs;

begin -- December_23
   Get_Input (Initial_State);
   Cost := 0;
   Least_Cost := Costs'Last;
   Solve (Initial_State, Cost, Least_Cost);
   Put_Line ("Part One Answer:" & Least_Cost'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_23;
