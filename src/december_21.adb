with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
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

   type Player_Arrays is array (Player_Indices) of Players;

   subtype Universes is Unsigned_64;

   type Results is array (Player_Indices) of Universes;

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
      subtype Throws is Positive range 1 .. 100;

      type Die_States is record
         Throw : Throws := Throws'First;
         Throw_Count : Natural := 0;
      end record; -- Die_States

      procedure Next_Position (Position : in out Board_Positions;
                               Die_State : in out Die_States) is

         -- The die is deterministic the next die state is dependent on the
         -- previous state, hence state variable Die_State.

      begin -- Next_Position
         Position :=
           (Position + Die_State.Throw - 1) mod Board_Positions'Last + 1;
         if Die_State.Throw < Throws'Last then
            Die_State.Throw := Die_State.Throw + 1;
         else
            Die_State.Throw := Throws'First;
         end if; -- Die_State.Throw < Throws'Last
         Die_State.Throw_Count := Die_State.Throw_Count + 1;
      end Next_Position;

      Die_State : Die_States;
      Player_Array : Player_Arrays := Player_Array_In;
      Game_Over : Boolean := False;
      Result : Natural := 0;

   begin -- Part_One
      while not Game_Over loop
         for P in Player_Indices loop
            if not Game_Over then
               for T in Positive range 1 .. 3 loop
                    Next_Position (Player_Array (P).Board_Position, Die_State);
               end loop; -- T in Positive range 1 .. 3
               Player_Array (P).Score := Player_Array (P).Score +
                 Player_Array (P).Board_Position;
               Game_Over := Player_Array (P).Score >= End_Score;
            end if; -- not Game_Over
         end loop; -- P in Player_Indices
      end loop; -- not Game_Over
      if Player_Array (1).Score >= End_Score then
         Result := Die_State.Throw_Count * Player_Array (2).Score;
      elsif Player_Array (2).Score >= End_Score then
         Result := Die_State.Throw_Count * Player_Array (1).Score;
      end if; -- Player_Array (P).Score >= End_Score
      return Result;
   end Part_One;

   procedure Part_Two_Play (Player_Array_In : in Player_Arrays;
                            Result : out Results) is

      -- Rather than directly playing the game the frequency of outcomes is
      -- calculated. For each outcome of three die rolls the frequency of that
      -- outcome is calculated as the product of the frequency of the previous
      -- state times the frequency of that roll total.

      Winning_Score : constant Scores := 21;

      -- For three throws of the Dirac Dice there are 27 possible outcomes
      -- ranging from 3 to 9 with frequencies as per the array below
      subtype Throw_Totals is Positive range 3 .. 9;

      subtype Frequencies is Unsigned_64 with
        Static_Predicate => Frequencies in 1 | 3 | 6 | 7;

      type Dirac_Dice is array (Throw_Totals) of Frequencies;

      Dirac_Die : constant Dirac_Dice := (1, 3, 6, 7, 6, 3, 1);

      type States is record
         Player_Array : Player_Arrays := Player_Array_In;
         Frequency : Universes := 1; -- One instance of start of game
         Turn : Player_Indices := 1; -- Player one plays first
      end record; -- States

      package Quantum_Queue_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (States);

      package Quantum_Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Quantum_Queue_Interface);

      function Next_Position (Board_Position : in Board_Positions;
                              Throw_Total : Throw_Totals)
                              return Board_Positions is

      begin -- Next_Position
         return (Board_Position + Throw_Total - 1) mod Board_Positions'Last + 1;
      end Next_Position;

      Quantum_Queue : Quantum_Queues.Queue;
      State, Next_State : States;

   begin -- Part_Two_Play
      Result := (others => 0);
      Quantum_Queue.Enqueue (State);
      while Quantum_Queue.Current_Use > 0 loop
         Quantum_Queue.Dequeue (State);
         if State.Turn = 1 then
            Next_State.Turn := 2;
         else
            Next_State.Turn := 1;
         end if; -- State.Turn = 1
         Next_State.Player_Array (Next_State.Turn) :=
           State.Player_Array (Next_State.Turn);
         -- transfer state of player who is not throwing die
         for Throw_Total in Throw_Totals loop
            Next_State.Player_Array (State.Turn).Board_Position :=
              Next_Position (State.Player_Array (State.Turn).Board_Position,
                             Throw_Total);
            Next_State.Player_Array (State.Turn).Score :=
              State.Player_Array (State.Turn).Score +
              Next_State.Player_Array (State.Turn).Board_Position;
            Next_State.Frequency := State.Frequency * Dirac_Die (Throw_Total);
            if Next_State.Player_Array (State.Turn).Score < Winning_Score then
               -- Continue playing this game instance
               Quantum_Queue.Enqueue (Next_State);
            else
               -- This gane instance is over, add the frequency of this outcome
               -- to the winning player's win count.
               Result (State.Turn) :=
                 Result (State.Turn) + Next_State.Frequency;
            end if; -- Next_State.Player_Array (State.Turn).Score ...
         end loop; -- Throw_Total in Throw_Totals
      end loop; --while Quantum_Queue.Current_Use > 0
      Put_Line ("Maximum games in play (queue usage):" &
                  Quantum_Queue.Peak_Use'Img);
   end Part_Two_Play;

   Player_Array : Player_Arrays;
   Result : Results;

begin -- December_21
   Get_Input (Player_Array);
   Put_Line ("Part One Answer:" & Part_One (Player_Array)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Part_Two_Play (Player_Array, Result);
   Put_Line ("Part Two Answer player One wins:" & Result (1)'Img &
            " player two wins:" & Result (2)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_21;
