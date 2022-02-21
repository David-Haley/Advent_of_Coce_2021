with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with  Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_23 is

   type Contents is (Wall, Empty, A, B, C, D);
   subtype Amphipods is Contents range A .. D;

   type X_Coordinates is (Hall_1, Hall_2, Room_A, Hall_3, Room_B, Hall_4,
                          Room_C, Hall_5, Room_D, Hall_6, Hall_7);

   Target_Table : constant array (Amphipods) of X_Coordinates :=
     (A => Room_A, B => Room_B, C => Room_C, D => Room_D);

   type Y_Coordinates is (Hall, Room_1, Room_2, Room_3, Room_4);
   subtype Y_Rooms is Y_Coordinates range Room_1 .. Room_4;

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
      -- To generalise for either two rooms or four rooms, rooms three and
      -- four are initialised to the solved stste.
      -- #A#B#C#D# Room_3
      -- #A#B#C#D# Room_4
      for Amphipod in Amphipods loop
         for Y in Y_Rooms range Room_3 .. Room_4 loop
            Initial_State (Target_Table (Amphipod), Y) := Amphipod;
         end loop; -- Y in Y_Rooms range Room_3 .. Room_4
      end loop; -- Amphipod in Amphipods
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_23.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Text);
      Assert ("#############" = To_String (Text), "Top wall not found");
      Get_Line (Input_File, Text);
      Assert ("#...........#" = To_String (Text), "Hallway not found");
      for Y in Y_Rooms range Room_1 .. Room_2 loop
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
      end loop; -- Y in Y_Rooms range Room_1 .. Room_2
      Get_Line (Input_File, Text);
      Assert ("  #########" = To_String (Text), "Bottom wall not found");
   end Get_Input;

   procedure Solve (State : in States;
                    Cost : in Costs;
                    Least_Cost : in out Costs) is

      type Coordinates is record
         X : X_Coordinates;
         Y : Y_Coordinates;
      end record; -- Coordinates

      type Moves is record
         Start, Finish : Coordinates;
      end record; -- Moves

      package Move_Lists is new Ada.Containers.Vectors (Positive, Moves);
      use Move_Lists;

      subtype Hall_Indices is Natural range 0 .. 6;

      Hall_Table : constant array (Hall_Indices) of X_Coordinates :=
        (Hall_1, Hall_2, Hall_3, Hall_4, Hall_5, Hall_6, Hall_7);
      -- This table is used to implement the rule that Amphipods cannot move to
      -- a position in front of a room.

      subtype Room_Indices is Natural range 0 .. 3;

      Room_Table : constant array (Room_Indices) of X_Coordinates :=
        (Room_A, Room_B, Room_C, Room_D);

      function Solved (State : in States) return Boolean is

      begin -- Solved
         -- Obvious cantidate for nested loops but this should be faster
         return State (Room_A, Room_1) = A and State (Room_A, Room_2) = A and
           State (Room_A, Room_3) = A and State (Room_A, Room_4) = A and
           State (Room_B, Room_1) = B and State (Room_B, Room_2) = B and
           State (Room_B, Room_3) = B and State (Room_B, Room_4) = B and
           State (Room_C, Room_1) = C and State (Room_C, Room_2) = C and
           State (Room_C, Room_3) = C and State (Room_C, Room_4) = C and
           State (Room_D, Room_1) = D and State (Room_D, Room_2) = D and
           State (Room_D, Room_3) = D and State (Room_D, Room_4) = D;
      end Solved;

      function Move_Cost (Move : in out Moves;
                          State : in States) return Costs is

         Cost_Table : constant array (Amphipods) of Costs :=
           (A => 1, B => 10, C => 100, D => 1000);

      begin -- Move_Cost
         if Move.Start.Y = Hall or Move.Finish.Y = Hall then
            -- Room to hall or hall to room move (Manhattan distance!)
            return (abs (X_Coordinates'Pos (Move.Finish.X) -
                      X_Coordinates'Pos (Move.Start.X)) +
                    abs (Y_Coordinates'Pos (Move.Finish.Y) -
                        Y_Coordinates'Pos (Move.Start.Y))) *
              Cost_Table (State (Move.Start.X, Move.Start.Y));
         else
            -- Room to room move
            return (abs (X_Coordinates'Pos (Move.Finish.X) -
                      X_Coordinates'Pos (Move.Start.X)) +
                    (Y_Coordinates'Pos (Move.Finish.Y) -
                         Y_Coordinates'Pos (Hall)) +
                    (Y_Coordinates'Pos (Move.Start.Y) -
                         Y_Coordinates'Pos (Hall))) *
              Cost_Table (State (Move.Start.X, Move.Start.Y));
         end if; --  Move.Start.Y = Hall or Move.Finish.Y = Hall
      end Move_Cost;

      function Valid_Move (Move : in Moves;
                           State : in States) return Boolean is

         -- Assumes that there ia an Amphipod at the start location and
         -- checks that the move is otherwise valid.

         function Valid_Start_Room (Move : in Moves;
                                    State : in States) return Boolean is

            -- Tests the rooms above are clear and the Amphipod is in the wrong
            -- room or alternatively the Amphipod is trapping one or more
            -- Amphipods which are in the wrong room.

            Amphipod : Amphipods := State (Move.Start.X, Move.Start.Y);

         begin -- Valid_Start_Room
            return (Move.Start.Y = Room_1 and then
                      (Target_Table (Amphipod) /= Move.Start.X or else
                       State (Move.Start.X, Room_2) /= Amphipod or else
                       State (Move.Start.X, Room_3) /= Amphipod or else
                       State (Move.Start.X, Room_4) /= Amphipod))
              or else
                (Move.Start.Y = Room_2 and then
                 State (Move.Start.X, Room_1) = Empty and then
                   (Target_Table (Amphipod) /= Move.Start.X or else
                    State (Move.Start.X, Room_3) /= Amphipod or else
                    State (Move.Start.X, Room_4) /= Amphipod))
              or else
                (Move.Start.Y = Room_3 and then
                 State (Move.Start.X, Room_1) = Empty and then
                 State (Move.Start.X, Room_2) = Empty and then
                   (Target_Table (Amphipod) /= Move.Start.X or else
                    State (Move.Start.X, Room_4) /= Amphipod))
              or else
                (Move.Start.Y = Room_4 and then
                 State (Move.Start.X, Room_1) = Empty and then
                 State (Move.Start.X, Room_2) = Empty and then
                 State (Move.Start.X, Room_3) = Empty and then
                 Target_Table (Amphipod) /= Move.Start.X);
         end Valid_Start_Room;

         function Valid_Finish_Room (Move : in Moves;
                                     State : in States) return Boolean is

            -- Assumes that the the finish is Empty. tests that the Finish is in
            -- the correct room which is either empty or only occupied by the
            -- correct Amphipods.

            Amphipod : Amphipods := State (Move.Start.X, Move.Start.Y);

         begin -- Valid_Finish_Room
            return Target_Table (Amphipod) = Move.Finish.X and then
              ((Move.Finish.Y = Room_1 and then
                State (Move.Finish.X, Room_2) = Amphipod and then
                State (Move.Finish.X, Room_3) = Amphipod and then
                State (Move.Finish.X, Room_4) = Amphipod)
               or else
                 (Move.Finish.Y = Room_2 and then
                  State (Move.Finish.X, Room_1) = Empty and then
                  State (Move.Finish.X, Room_3) = Amphipod and then
                  State (Move.Finish.X, Room_4) = Amphipod)
               or else
                 (Move.Finish.Y = Room_3 and then
                  State (Move.Finish.X, Room_1) = Empty and then
                  State (Move.Finish.X, Room_2) = Empty and then
                  State (Move.Finish.X, Room_4) = Amphipod)
               or else
                 (Move.Finish.Y = Room_4 and then
                  State (Move.Finish.X, Room_1) = Empty and then
                  State (Move.Finish.X, Room_2) = Empty and then
                  State (Move.Finish.X, Room_3) = Empty));
         end Valid_Finish_Room;

         pragma Inline_Always (Valid_Start_Room, Valid_Finish_Room);

         Result : Boolean;
         X_Src : X_Coordinates;

      begin -- Valid_Move
         Result := State (Move.Finish.X, Move.Finish.Y) = Empty
         -- Finish is clear
           and then
             ((Move.Start.Y /= Hall and then
               Valid_Start_Room (Move, State) and then
                 (Move.Finish.Y = Hall or else Valid_Finish_Room (Move, State)))
              or else
                (Move.Start.Y = Hall and then Valid_Finish_Room (Move, State)));
         if Result then
            -- Check that hallway is clear
            if Move.Start.X < Move.Finish.X then
               if Move.Start.Y = Hall then
                  X_Src := X_Coordinates'Succ (Move.Start.X);
               else
                  X_Src := Move.Start.X;
               end if; -- Move.Start.Y = Hall
               for X in X_Coordinates range X_Src .. Move.Finish.X loop
                  Result := Result and State (X, Hall) = Empty;
               end loop; -- X in X_Coordinates range X_Src .. Move.Finish.X
            else
               if Move.Start.Y = Hall then
                  X_Src := X_Coordinates'Pred (Move.Start.X);
               else
                  X_Src := Move.Start.X;
               end if; -- Move.Start.Y = Hall
               for X in X_Coordinates range Move.Finish.X .. X_Src loop
                  Result := Result and State (X, Hall) = Empty;
               end loop; -- X in X_Coordinates range Move.Finish.X .. X_Src
            end if; -- Move.Start.X < Move.Finish.X
         end if; -- Result
         return Result;
      end Valid_Move;

      pragma Inline_Always (Solved, Move_Cost, Valid_Move);

      Next_State : States;
      Move : Moves;
      Move_List : Move_Lists.Vector;
      Room_to_Room : Boolean;

   begin -- Solve
      --  Put_Line (Cost'Img);
      --  Put (State);
      if Solved (State) then
         if Cost < Least_Cost then
            Least_Cost := Cost;
         end if; -- Cost < Least_Cost
      elsif Cost < Least_Cost then
         -- the above is huresuic pruning, if the search costs more than a
         -- solution that has already been found, don't proceed
         -- Explore moves from room to room or room to hall
         for R_Start in Room_Indices loop
            Room_to_Room := False;
            Move.Start.X := Room_Table (R_Start);
            for Y_Start in Y_Rooms loop
               Move.Start.Y := Y_Start;
               if State (Move.Start.X, Move.Start.Y) in Amphipods then
                  -- Testing for room to room move first avoids a two step move
                  -- to hall than move to room. It was coded on this basis on
                  -- assumption that part two may have involved counting moves.
                  Move.Finish.X :=
                    Target_Table (State (Move.Start.X, Move.Start.Y));
                  for Y_Finish in Y_Rooms loop
                     Move.Finish.Y := Y_Finish;
                     if Valid_Move (Move, State) then
                        Room_to_Room := True;
                        Append (Move_List, Move);
                     end if; -- Valid_Move (Move, State)
                  end loop; -- Y_Finish in Y_Rooms
                  if not Room_to_Room then
                     Move.Finish.Y := Hall;
                     for H_Finish in Hall_Indices loop
                        Move.Finish.X := Hall_Table (H_Finish);
                        if Valid_Move (Move, State) then
                           Append (Move_List, Move);
                        end if; -- Valid_Move (Move, State)
                     end loop; -- H_Finish in Hall_Indices
                  end if; -- not Room_to_Room
               end if; -- State (Move.Start.X, Move.Start.Y) in Amphipods
            end loop; -- Y_Start in Y_Rooms
         end loop; -- R_Start in Room_Indices
         -- Explore moves from hall to rooms
         Move.Start.Y := Hall;
         for H_Start in Hall_Indices loop
            Move.Start.X := Hall_Table (H_Start);
            if State (Move.Start.X, Move.Start.Y) in Amphipods then
               Move.Finish.X :=
                 Target_Table (State (Move.Start.X, Move.Start.Y));
               for Y_Finish in Y_Rooms loop
                  Move.Finish.Y := Y_Finish;
                  if Valid_Move (Move, State) then
                     Append (Move_List, Move);
                  end if; -- Valid_Move (Move, State)
               end loop; -- Y_Finish in Y_Rooms
            end if; -- State (Move.Start.X, Move.Start.Y) in Amphipods
         end loop; -- H_Start in Hall_Indices
         -- Move list complete descend to next level
         for M in Iterate (Move_List) loop
            Move := Move_List (M);
            Next_State := State;
            Next_State (Move.Finish.X, Move.Finish.Y) :=
              Next_State (Move.Start.X, Move.Start.Y);
            Next_State (Move.Start.X, Move.Start.Y) := Empty;
            Solve (Next_State, Cost + Move_Cost (Move, State), Least_Cost);
         end loop; -- M in Iterate (Move_List)
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
   -- Part 2 initialisation
   -- Move contents of Room_2 to Room_4
   Initial_State (Room_A, Room_4) := Initial_State (Room_A, Room_2);
   Initial_State (Room_B, Room_4) := Initial_State (Room_B, Room_2);
   Initial_State (Room_C, Room_4) := Initial_State (Room_C, Room_2);
   Initial_State (Room_D, Room_4) := Initial_State (Room_D, Room_2);
   -- Initialise Room_2 and Room_3 as per problem description
   Initial_State (Room_A, Room_2) := D;
   Initial_State (Room_A, Room_3) := D;
   Initial_State (Room_B, Room_2) := C;
   Initial_State (Room_B, Room_3) := B;
   Initial_State (Room_C, Room_2) := B;
   Initial_State (Room_C, Room_3) := A;
   Initial_State (Room_D, Room_2) := A;
   Initial_State (Room_D, Room_3) := C;
   -- Solve part 2
   Cost := 0;
   Least_Cost := Costs'Last;
   Solve (Initial_State, Cost, Least_Cost);
   Put_Line ("Part Two Answer:" & Least_Cost'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_23;
