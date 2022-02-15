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

      type Coordinates is record
         X : X_Coordinates;
         Y : Y_Coordinates;
      end record; -- Coordinates

      type Moves is record
         Start, Finish : Coordinates;
      end record; -- Moves

      package Move_Lists is new Ada.Containers.Vectors (Positive, Moves);
      use Move_Lists;

      subtype Y_Rooms is Y_Coordinates range Room_1 .. Room_2;

      subtype Hall_Indices is Natural range 0 .. 6;

      Hall_Table : constant array (Hall_Indices) of X_Coordinates :=
        (Hall_1, Hall_2, Hall_3, Hall_4, Hall_5, Hall_6, Hall_7);

      subtype Room_Indices is Natural range 0 .. 3;

      Room_Table : constant array (Room_Indices) of X_Coordinates :=
        (Room_A, Room_B, Room_C, Room_D);

      Target_Table : constant array (Amphipods) of X_Coordinates :=
        (A => Room_A, B => Room_B, C => Room_C, D => Room_D);

      function Solved (State : in States) return Boolean is

      begin -- Solved
         return State (Room_A, Room_1) = A and State (Room_A, Room_2) = A and
           State (Room_B, Room_1) = B and State (Room_B, Room_2) = B and
           State (Room_C, Room_1) = C and State (Room_C, Room_2) = C and
           State (Room_D, Room_1) = D and State (Room_D, Room_2) = D;
      end Solved;

      function Move_Cost (Move : in Moves;
                          State : in States) return Costs is

         Cost_Table : constant array (Amphipods) of Costs :=
           (A => 1, B => 10, C => 100, D => 1000);

      begin -- Move_Cost
         if Move.Start.Y = Hall or Move.Finish.Y = Hall then
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

         Result : Boolean;
         X_Src : X_Coordinates;
         Content : Contents := State (Move.Start.X, Move.Start.Y);

      begin -- Valid_Move
         Result := Content in Amphipods and then
         -- Amphipod at start of move
           State (Move.Finish.X, Move.Finish.Y) = Empty and then
         -- Finish is clear
           (Move.Finish.Y = Hall or else
            (Move.Finish.X = Target_Table (Content)
            -- Finish hall or correct room
            and then
              ((Move.Finish.Y = Room_2 and
                    State (Move.Finish.X, Room_1) = Empty)
                 -- if Finish is Room_2 then Room_1 must be Empty
               or else
                 (Move.Finish.Y = Room_1 and State (Move.Finish.X, Room_2) =
                      Content)))) and then
         -- If Finish is Room_1 then Room_2 must be occupied by the correct
         -- Amphipod.
           (Move.Start.Y /= Room_2 or else
            State (Move.Start.X, Room_1) = Empty) and then
         -- If Start is Room_2 then Room_1 must be clear
           (Move.Start.Y = Hall or else Target_Table (Content) /= Move.Start.X
            -- Not in correct room
            or else
              (Move.Start.Y = Room_1 and then
               Target_Table (State (Move.Start.X, Room_2)) /= Move.Start.X));
         -- alternatively in correct room and trapping wrong Amphipod for room.
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
                  end if; -- not Room_to_Room then
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
         --  Put_Line ("Length:" & Count_Type'Image (Length (Move_List)));
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
end December_23;
