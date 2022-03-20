with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Interfaces; use Interfaces;
with DJH.Execution_Time;

procedure December_21_Part_Two_Recursive is

   subtype Board_Positions is Positive range 1 .. 10;

   subtype Scores is Natural;

   Winning_Score : constant Scores := 21; -- Part Two only

   subtype Score_Indices is Scores range Scores'First .. Winning_Score - 1;

   subtype Player_Indices is Positive range 1 .. 2;

   type Players is record
      Board_Position : Board_Positions;
      Score : Scores := 0;
   end record; -- Players

   type Player_Arrays is array (Player_Indices) of Players;

   subtype Universes is Unsigned_64;

   type Results is array (Player_Indices) of Universes;

   type Cache_Elements is record
      Defined : Boolean := False;
      Result : Results := (others => 0);
   end record; -- Cache_Elements

   type Caches is array (Player_Indices,
                         Board_Positions, Score_Indices,
                         Board_Positions, Score_Indices) of Cache_Elements;

   type States is record
      Player_Array : Player_Arrays;
      Turn : Player_Indices := 1; -- Player one plays first
   end record; -- States

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

   function Part_Two_Play (State : in States;
                           Cache : in out Caches) return Results is

      -- Rather than directly playing the game the frequency of outcomes is
      -- calculated. For each outcome of three die rolls the frequency of that
      -- outcome is calculated as the product of the frequency of the previous
      -- state times the frequency of that roll total.
      -- For three throws of the Dirac Dice there are 27 possible outcomes
      -- ranging from 3 to 9 with frequencies as per the array below

      subtype Throw_Totals is Positive range 3 .. 9;

      subtype Frequencies is Universes with
        Static_Predicate => Frequencies in 1 | 3 | 6 | 7;

      type Dirac_Dice is array (Throw_Totals) of Frequencies;

      Dirac_Die : constant Dirac_Dice := (1, 3, 6, 7, 6, 3, 1);

      function Next_Position (Board_Position : in Board_Positions;
                              Throw_Total : Throw_Totals)
                              return Board_Positions is

      begin -- Next_Position
         return (Board_Position + Throw_Total - 1) mod Board_Positions'Last + 1;
      end Next_Position;

      Next_State : States := State; -- transfer player who is not rolling dice
      Result : Results := (others => 0);
      Returned : Results;

   begin -- Part_Two_Play
      if Cache (State.Turn, State.Player_Array (1).Board_Position,
                State.Player_Array (1).Score,
                State.Player_Array (2).Board_Position,
                State.Player_Array (2).Score).Defined then
         -- State already calculated, result in Cache
         return Cache (State.Turn, State.Player_Array (1).Board_Position,
                       State.Player_Array (1).Score,
                       State.Player_Array (2).Board_Position,
                       State.Player_Array (2).Score).Result;
      else
         -- State not previously calculated, put in Cache after calls
         if State.Turn = 1 then
            Next_State.Turn := 2;
         else
            Next_State.Turn := 1;
         end if; -- State.Turn = 1
         for Throw_Total in Throw_Totals loop
            Next_State.Player_Array (State.Turn).Board_Position :=
              Next_Position (State.Player_Array (State.Turn).Board_Position,
                             Throw_Total);
            Next_State.Player_Array (State.Turn).Score :=
              State.Player_Array (State.Turn).Score +
              Next_State.Player_Array (State.Turn).Board_Position;
            if Next_State.Player_Array (State.Turn).Score < Winning_Score then
               -- Continue playing this game instance
               Returned := Part_Two_Play (Next_State, Cache);
               Result (1) := Result (1) +
                 Returned (1) * Dirac_Die (Throw_Total);
               Result (2) := Result (2) +
                 Returned (2) * Dirac_Die (Throw_Total);
            else
               -- This game instance is over, add the frequency of this outcome
               -- to the winning player's win count.
               Result (State.Turn) :=
                 Result (State.Turn) + Dirac_Die (Throw_Total);
            end if; -- Next_State.Player_Array (State.Turn).Score ...
         end loop; -- Throw_Total in Throw_Totals
         Cache (State.Turn, State.Player_Array (1).Board_Position,
                       State.Player_Array (1).Score,
                       State.Player_Array (2).Board_Position,
                       State.Player_Array (2).Score).Result := Result;
         Cache (State.Turn, State.Player_Array (1).Board_Position,
                State.Player_Array (1).Score,
                State.Player_Array (2).Board_Position,
                State.Player_Array (2).Score).Defined := True;
         return Result;
      end if; -- Cache ... Defined
   end Part_Two_Play;

   Player_Array : Player_Arrays;
   Result : Results;
   Cache : Caches;
   -- N.B. requires linker switch -Xlinker --stack=0x250000,0x1000
   State : States;

begin -- December_21_Part_Two_Recursive
   Get_Input (Player_Array);
   Put_Line ("Part One Answer:" & Part_One (Player_Array)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   --  Cache := new Caches;
   State.Player_Array := Player_Array;
   Result := Part_Two_Play (State, Cache);
   Put_Line ("Part Two Answer player One wins:" & Result (1)'Img &
               " player two wins:" & Result (2)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_21_Part_Two_Recursive;
