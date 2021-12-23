with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Deterninistic_Die; use Deterninistic_Die;
with Interfaces; use Interfaces;
with DJH.Execution_Time;

procedure December_21 is

   subtype Board_Positions is Positive range 1 .. 10;

   subtype Scores is Natural;

   subtype Player_Indices is Positive range 1 .. 2;

   type Players is record
      Board_Position : Board_Positions;
      Score : Scores := 0;
   end record; -- Players

   subtype Universes is Unsigned_64;

   type Player_Arrays is array (Player_Indices) of Players;

   type Player_Results is record
      Wins, Losses : Universes := 0;
   end record; -- Player_Results
   type Results is array (Player_Indices) of Player_Results;

   subtype Turns is Natural;

   package Turn_Stores is new Ada.Containers.Ordered_Maps (Turns, Results);
   use Turn_Stores;

   procedure Get_Input (Player_Array : out Player_Arrays) is

      -- december_21 [Input_File_Name]

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Player_I : Player_Indices;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_21.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      for P in Player_Indices loop
         Start_At := 1;
         Get_Line (Input_File, Text);
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Player_I := Player_Indices'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Player_Array (Player_I).Board_Position :=
           Board_Positions'Value (Slice (Text, First, Last));
      end loop; -- P in Player_Indices
      Close (Input_File);
   end Get_Input;

   function Part_One (Player_Array_In : in Player_Arrays) return Natural is

      End_Score : constant  Natural := 1000;

      function Next_Position (Position : in Board_Positions;
                              Throw : Die_Values)
                              return Board_Positions is

      begin -- Next_Position
         return (Position + Throw - 1) mod 10 + 1;
      end Next_Position;

      Player_Array : Player_Arrays := Player_Array_In;
      Game_Over : Boolean := False;
      Result : Natural := 0;

   begin -- Part_One
      while not Game_Over loop
         for P in Player_Indices loop
            if not  Game_Over then
               for T in Positive range 1 .. 3 loop
                  Player_Array (P).Board_Position :=
                    Next_Position (Player_Array (P).Board_Position, Roll_Die);
               end loop; -- T in Positive range 1 .. 3
               Player_Array (P).Score := Player_Array (P).Score +
                 Player_Array (P).Board_Position;
               Game_Over := Player_Array (P).Score >= End_Score;
            end if; -- not Game_Over
         end loop; -- P in Player_Indices
      end loop; -- not Game_Over
      if Player_Array (1).Score >= End_Score then
         Result := Die_Throws * Player_Array (2).Score;
      elsif Player_Array (2).Score >= End_Score then
         Result := Die_Throws * Player_Array (2).Score;
      end if; -- Player_Array (P).Score >= End_Score
      return Result;
   end Part_One;

   procedure Part_Two_Play (Player_Array : in Player_Arrays;
                           Turn_Store : out Turn_Stores.Map) is

      Winning_Score : constant Scores := 21;

      -- for three throws of the Dirac Dice there are 27 possible outcomes
      -- ranging from 3 to 9 with frequencies as per the array below
      subtype Frequencies is Unsigned_64 with
        Static_Predicate => Frequencies in 1 | 3 | 6 | 7;
      subtype Throw_Totals is Positive range 3 .. 9;
      type Dirac_Dice is array (Throw_Totals) of Frequencies;

      Dirac_Die : constant Dirac_Dice := (1, 3, 6, 7, 6, 3, 1);

      type States is record
         Turn : Turns;
         Board_Position : Board_Positions;
         Score : Natural;
         Score_Frequency : Universes;
      end record; -- States

      package Quantum_Queue_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (States);

      package Quantum_Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Quantum_Queue_Interface);
      use Quantum_Queues;

      function Next_Position (Board_Position : in Board_Positions;
                              Throw_Total : Throw_Totals)
                              return Board_Positions is

      begin -- Next_Position
         return (Board_Position + Throw_Total - 1) mod 10 + 1;
      end Next_Position;

      Quantum_Queue : Quantum_Queues.Queue;
      State, Next_State : States;
      Result : constant Results := ((0, 0), (0, 0));

   begin -- Part_Two_Play
      for P in Player_Indices loop
         Put_Line ("Player:" & P'Img);
         State.Turn := 0;
         State.Board_Position := Player_Array (P).Board_Position;
         State.Score := 0;
         State.Score_Frequency := 1;
         Quantum_Queue.Enqueue (State);
         while Quantum_Queue.Current_Use > 0 loop
            Quantum_Queue.Dequeue (State);
            if State.Score < Winning_Score then
               if not Contains (Turn_Store, State.Turn) then
                  Include (Turn_Store, State.Turn, Result);
               end if; -- not Contains (Turn_Store, State.Turn)
               Turn_Store (State.Turn) (P).Losses :=
                 Turn_Store (State.Turn) (P).Losses + State.Score_Frequency;
               Next_State.Turn := State.Turn + 1;
               for Throw_Total in Throw_Totals loop
                  Next_State.Board_Position :=
                    Next_Position (State.Board_Position, Throw_Total);
                  Next_State.Score := State.Score + Next_State.Board_Position;
                  Next_State.Score_Frequency := State.Score_Frequency *
                    Dirac_Die (Throw_Total);
                  Quantum_Queue.Enqueue (Next_State);
               end loop;
            else
               if not Contains (Turn_Store, State.Turn) then
                  Include (Turn_Store, State.Turn, Result);
               end if; -- not Contains (Turn_Store, State.Turn)
               Turn_Store (State.Turn) (P).Wins :=
                 Turn_Store (State.Turn) (P).Wins + State.Score_Frequency;
            end if; -- State.Score < Winning_Score
         end loop; --while Quantum_Queue.Current_Use > 0
      end loop; -- P in Player_Indices
   end Part_Two_Play;

   function Part_Two (Turn_Store : in Turn_Stores.Map) return Universes is

      Win_Universe : array (Player_Indices) of Universes := (others => 0);
      Total : Results;

   begin -- Part_Two
      for T in Iterate (Turn_Store) loop
         Put_Line (Key (T)'Img & Turn_Store (T) (1).Wins'Img & Turn_Store (T) (1).Losses'Img & Turn_Store (T) (2).Wins'Img & Turn_Store (T) (2).Losses'Img);
         if Turn_Store (T) (1).Wins > 0 or  Turn_Store (T) (2).Wins > 0 then
            for P in Player_Indices loop
               Total (P).Wins := Total (P).Wins + Turn_Store (T) (P).Wins;
               Total (P).Losses :=
                 Total (P).Losses + Turn_Store (T) (P).Losses;
            end loop; -- P in Player_Indices
         end if; -- Turn_Store (T) (1).Wins > 0 or  Turn_Store (T) (2).Wins > 0
      end loop; -- T in Iterate (Turn_Store)
      Put_Line (Total (1).Wins'Img & Total (1).Losses'Img & Total (2).Wins'Img & Total (2).Losses'Img);
      if Total (1).Wins * Total (2).Losses > Total (2).Wins * Total (1).Losses then
         return Total (1).Wins * Total (2).Losses;
      else
         return Total (2).Wins * Total (1).Losses;
      end if; -- Total.Wins (1) * Total.Losses (2) > Total.Wins (2) * Total.Losses (1)
   end Part_Two;


   Player_Array : Player_Arrays;
   Turn_Store : Turn_Stores.Map;

begin -- December_21
   Get_Input (Player_Array);
   Put_Line ("Part One Answer:" & Part_One (Player_Array)'Img);
   Part_Two_Play (Player_Array, Turn_Store);
   Put_Line ("Part Two Answer:" & Part_Two (Turn_Store)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_21;
