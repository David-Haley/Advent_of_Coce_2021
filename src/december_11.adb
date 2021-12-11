with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with NT_Console;
with DJH.Execution_Time;

procedure December_11 is

   Animation_Delay : constant Duration := 0.1;
   Part_One_Steps : constant Positive := 100;

   subtype Coordinates is Natural;
   subtype Extended_Coordinated is Integer range -1 .. Coordinates'Last;

   type Octopus_Positions is record
      X, Y : Coordinates;
   end record; -- OctoPus_Positions

   function "<" (Left, Right : OctoPus_Positions) return Boolean is

   begin -- "<"
      return Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X);
   end "<";

   function "=" (Left, Right : OctoPus_Positions) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   subtype Energies is Natural range 0 .. 9;

   type Octopi is record
      Energy : Energies;
      Flashed : Boolean := False;
   end record; -- Octopi

   package Octopus_Stores is new
     Ada.Containers.Ordered_Maps (Octopus_Positions, Octopi);
   use Octopus_Stores;

   package Screen is new NT_Console;
   use Screen;

   procedure Get_Input (Octopus_Store : out Octopus_Stores.Map) is

      -- december_11 [Input_File_Name]

      Input_File : File_Type;
      Text : Unbounded_String;
      Octopus_Position : OctoPus_Positions;
      Octopus : Octopi;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_11.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Octopus_Position.Y := 0;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for I in Positive range 1 .. Length (Text) loop
            Octopus_Position.X := I - 1;
            Octopus.Energy := Energies'Value (Slice (Text, I, I));
            Include (Octopus_Store, Octopus_Position, Octopus);
         end loop;
         Octopus_Position.Y := Octopus_Position.Y + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   function Find_Flashes (Octopus_Store : in out Octopus_Stores.Map)
                          return Natural is

      subtype Offsets is Extended_Coordinated range -1 .. 1;

      package Octopus_Queue_Interfaces is new
        Ada.Containers.Synchronized_Queue_Interfaces (OctoPus_Positions);

      package Octopus_Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Octopus_Queue_Interfaces);

      subtype Differences is Extended_Coordinated range -1 .. 1;

      function Valid_Octopus (X, Y : in Extended_Coordinated;
                              Octopus_Store : in Octopus_Stores.Map)
                              return Boolean is

      begin -- Valid_Octopus
         return (X >= Coordinates'First and Y >= Coordinates'First) and then
           Contains (Octopus_Store, (X, Y));
      end Valid_Octopus;

      Octopus_Queue : Octopus_Queues.Queue;
      Octopus, Next_Octopus : OctoPus_Positions;
      Flash_Count : Natural := 0;

   begin -- Find_Flashes
      for O in Iterate (Octopus_Store) loop
         if Octopus_Store (O).Energy < Energies'Last then
            Octopus_Store (O).Energy := Octopus_Store (O).Energy + 1;
         else
            Octopus_Queue.Enqueue(Key (O));
         end if; -- Octopus_Store (O).Energy < Energies'Last
      end loop; -- O in Iterate (Octopus_Store)
      while Octopus_Queue.Current_Use > 0 loop
         Octopus_Queue.Dequeue (Octopus);
         if not Octopus_Store (Octopus).Flashed then
            Octopus_Store (Octopus).Flashed := True;
            Flash_Count := Flash_Count + 1;
            for Dx in Offsets loop
               for Dy in Offsets loop
                  if (Dx /= 0 or Dy /= 0) and then
                    Valid_Octopus (Octopus.X + Dx, Octopus.Y + Dy,
                                   Octopus_Store) then
                     Next_Octopus := (Octopus.X + Dx, Octopus.Y + Dy);
                     if Octopus_Store (Next_Octopus).Energy < Energies'Last then
                        Octopus_Store (Next_Octopus).Energy :=
                          Octopus_Store (Next_Octopus).Energy + 1;
                     else
                        Octopus_Queue.Enqueue (Next_Octopus);
                     end if; -- Octopus_Store (Octopus).Energy < Energies'Last
                  end if; -- (Dx /= 0 or Dy /= 0) and then ...
               end loop; -- Dy in Offsets
            end loop; -- Dx in Offsets
         end if; -- not Octopus_Stores (Octopus).Flashed
      end loop; -- Octopus_Queue.Current_Use > 0
      return Flash_Count;
   end Find_Flashes;

   function Part_One (Octopus_Store : in out Octopus_Stores.Map;
                      Animate : in Boolean := False) return Natural is

      Total_Flash_Count : Natural := 0;

   begin -- Part_One
      for Step in Positive range 1 .. 100 loop
         Total_Flash_Count := Total_Flash_Count +
           Find_Flashes (Octopus_Store);
         if Animate then
            Goto_XY (X_Pos'First, Y_Pos'First);
            Put ("Step:" & Step'Img);
            for O in Iterate (Octopus_Store) loop
               Goto_XY (Key (O).X, Key (O).Y + 1);
               if Octopus_Store (O).Flashed then
                  Octopus_Store (O).Flashed := False;
                  Octopus_Store (O).Energy := 0;
                  Set_Foreground (White);
                  Put ('0');
                  Set_Foreground (Gray);
               else
                  Put (Trim (Energies'Image (Octopus_Store (O).Energy),Left));
               end if; -- Octopus_Store (O).Flashed
            end loop; -- O in Iterate (Octopus_Store)
            delay Animation_Delay;
         else
            for O in Iterate (Octopus_Store) loop
               if Octopus_Store (O).Flashed then
                  Octopus_Store (O).Flashed := False;
                  Octopus_Store (O).Energy := 0;
               end if; -- O in Iterate (Octopus_Store)
            end loop; -- O in Iterate (Octopus_Store)
         end if; -- Animate
      end loop; -- I in Positive range 1 .. 100
      return Total_Flash_Count;
   end Part_One;

   function Part_Two (Octopus_Store : in out Octopus_Stores.Map;
                      Animate : in Boolean := False) return Natural is

      function All_Flash (Octopus_Store : in Octopus_Stores.Map)
                          return Boolean is

         Result : Boolean := True;

      begin -- All_Flash
         for O in Iterate (Octopus_Store) loop
            Result := Result and Octopus_Store (O).Flashed;
         end loop; -- O in Iterate (Octopus_Store)
         return Result;
      end All_Flash;

      Total_Flash_Count : Natural;
      Step : Positive := Part_One_Steps;
      Finished : Boolean;

   begin -- Part_Two
      Finished := All_Flash (Octopus_Store);
      while not Finished loop
         Step := Step + 1;
         Total_Flash_Count := Find_Flashes (Octopus_Store);
         -- Need to store unused result, not accumulated, because if it takes
         -- many cycles to finish overflow may occur;
         Finished := All_Flash (Octopus_Store);
         if Animate then
            Goto_XY (X_Pos'First, Y_Pos'First);
            Put ("Step:" & Step'Img);
            for O in Iterate (Octopus_Store) loop
               Goto_XY (Key (O).X, Key (O).Y + 1);
               if Octopus_Store (O).Flashed then
                  Octopus_Store (O).Flashed := False;
                  Octopus_Store (O).Energy := 0;
                  Set_Foreground (White);
                  Put ('0');
                  Set_Foreground (Gray);
               else
                  Put (Trim (Energies'Image (Octopus_Store (O).Energy),Left));
               end if; -- Octopus_Store (O).Flashed
            end loop; -- O in Iterate (Octopus_Store)
            delay Animation_Delay;
         else
            for O in Iterate (Octopus_Store) loop
               if Octopus_Store (O).Flashed then
                  Octopus_Store (O).Flashed := False;
                  Octopus_Store (O).Energy := 0;
               end if; -- O in Iterate (Octopus_Store)
            end loop; -- O in Iterate (Octopus_Store)
         end if; -- Animate
      end loop; -- not Finished
      return Step;
   end Part_Two;

   Octopus_Store : Octopus_Stores.Map;
   Ch : character;
   Animate : Boolean;
   Part_One_Answer, Part_Two_Answer : Natural;

begin -- December_11
   Get_Input (Octopus_Store);
   Put ("Run Animation y | n : ");
   Ch := Get_Key;
   Animate := Ch = 'Y' or Ch = 'y';
   if Animate then
      Clear_Screen;
      Set_Cursor (False);
      Part_One_Answer := Part_One (Octopus_Store, Animate);
      Goto_XY (X_Pos'First, Y_Pos'Last - 3);
      Put ("Part One Answer:" & Part_One_Answer'Img);
      delay 3.0;
      Part_Two_Answer := Part_Two (Octopus_Store, Animate);
      Goto_XY (X_Pos'First, Y_Pos'Last - 2);
      Put ("Part Two Answer:" & Part_Two_Answer'Img);
      Goto_XY (X_Pos'First, Y_Pos'Last - 1);
      Djh.Execution_Time.Put_CPU_Time;
      Set_Cursor (True);
   else
      New_Line;
      Put_Line ("Part One Answer:" & Part_One (Octopus_Store)'Img);
      Put_Line ("Part Two Answer:" & Part_Two (Octopus_Store)'Img);
      Djh.Execution_Time.Put_CPU_Time;
   end if; -- Animate
end December_11;
