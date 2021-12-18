with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with DJH.Execution_Time;

procedure December_17 is

   subtype X_Coordinates is Natural;
   subtype Y_Coordinates is Integer;

   type Targets is record
      X1, X2 : X_Coordinates;
      Y1, Y2 : Y_Coordinates;
   end record; -- Targets

   subtype X_Velocities is Natural;
   subtype Y_Velocities is Integer;

   type Probes is Record
      X : X_Velocities;
      Y : Y_Velocities;
      Vx : X_Velocities;
      Vy : Y_Velocities;
   end record; -- Probes

   type Real is digits 15;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions (Real);
   use Functions;

   procedure Get_Input (Target : out Targets) is

      Integer_Set : constant Character_Set := To_Set ("-0123456789");
      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Xt : X_Coordinates;
      Yt : Y_Coordinates;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_17.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Text);
      Start_At := 1;
      Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
      Target.X1 := X_Coordinates'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
      Target.X2 := X_Coordinates'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Find_Token (Text,Integer_Set, Start_At, Inside, First, Last);
      Target.Y1 := Y_Coordinates'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
      Target.Y2 := Y_Coordinates'Value (Slice (Text, First, Last));
      Close (Input_File);
      if Target.X1 > Target.X2 then
         Xt := Target.X2;
         Target.X2 := Target.X1;
         Target.X1 := Xt;
      end if; -- Target.X1 > Target.X2
      if Target.Y1 < Target.Y2 then
         Yt := Target.Y2;
         Target.Y2 := Target.Y1;
         Target.Y1 := Yt;
      end if; -- Target.Y1 < Target.Y2
   end Get_Input;

   function Target_Hit (Target : in Targets;
                        Probe : in Probes) return Boolean is

   begin -- Target_Hit
      return Target.X1 <= Probe.X and Probe.x <= Target.X2 and
        Target.Y1 >= Probe.Y and Probe.Y >= Target.Y2;
   end Target_Hit;

   procedure Simulate (Target : in Targets;
                       Probe : in out Probes;
                       Vx : in X_Velocities;
                       Vy : in Y_Velocities;
                       Success : out Boolean;
                       Y_Max : out Y_Coordinates) is

      procedure Initialise (Vx : in X_Velocities;
                            Vy : in Y_Velocities;
                            Probe : out Probes) is

      begin -- Initialise
         Probe.X := 0;
         Probe.Y := 0;
         Probe.Vx := Vx;
         Probe.Vy := Vy;
      end Initialise;

      Procedure Update (Probe : in out Probes) is

      begin -- Update
         Probe.X := Probe.X + Probe.Vx;
         Probe.Y := Probe.Y + Probe.Vy;
         if Probe.Vx > 0 then
            Probe.Vx := Probe.Vx - 1;
         end if; -- Probe.Vx > 0
         Probe.Vy := Probe.Vy - 1;
      end Update;

   begin -- Simulate
      Y_Max := Integer'First;
      Success := False;
      Initialise (Vx, Vy, Probe);
      while not Success and Probe.X < Target.X2 and Probe.Y > Target.Y2 loop
         Update (Probe);
         Success := Target_Hit (Target, Probe);
         if Probe.Y > Y_Max then
            Y_max := Probe.Y;
         end if; -- Probe.Y > Y_Max
      end loop; -- not Success and Probe.X < Target.X2 and Probe.Y > Target.Y2
   end Simulate;

   procedure Find_Target (Target : in Targets;
                          Probe : in out Probes;
                          Vx_Hit : out X_Velocities;
                          Vy_Hit : out Y_Velocities;
                          Y_Max : out Y_Coordinates) is

      procedure Estimate (Target : in Targets;
                          Vx : out X_Velocities;
                          Vy : out Y_Velocities) is

         T : Real;

      begin -- Estimate
         -- N.B. acceleration is -1
         T := Sqrt (Real (2 * Target.X1));
         Vx := X_Velocities (T);
         Vy := Y_Velocities (Real (Target.Y1) + 0.5 * T ** 2);
      end Estimate;

      Success : Boolean := False;


   begin -- Find_Target
      Estimate (Target, Vx_Hit, Vy_Hit);
      while not Success loop
         Simulate (Target, Probe, Vx_Hit, Vy_Hit, Success, Y_Max);
         if Probe.X < Target.X1 then
            Vx_Hit := Vx_Hit + 1;
         elsif Probe.X > Target.X2 then
            Vx_Hit := Vx_Hit - 1;
         end if; -- Probe.X < Target.X1
         if Probe.Y < Target.Y2 then
            Vy_Hit := Vy_Hit - 1;
         elsif Probe.Y > Target.Y1 then
            Vy_Hit := Vy_Hit + 1;
         end if; -- Probe.Y < Target.Y2
      end loop; -- not Target_Hit (Target, Probe)
   end Find_Target;

   procedure Optomise (Target : in Targets;
                       Probe : in out Probes;
                       Vx_In : in X_Velocities;
                       Vy_in : in Y_Velocities;
                       Y_Max : in out Y_Coordinates;
                       Pair_Count : out Natural) is

      package Vx_Stores is new Ada.Containers.Vectors (Positive, X_Velocities);
      use Vx_Stores;

      package Vy_Stores is new Ada.Containers.Vectors (Positive, Y_Velocities);
      use Vy_Stores;

      procedure Simulate_X (Target : in Targets;
                            Probe : in out Probes;
                            Vx : in X_Velocities;
                            Success : out Boolean) is

      begin -- Simulate_X
         Success := False;
         Probe.X := 0;
         Probe.Vx := Vx;
         while not Success and Probe.X < Target.X2 loop
            Probe.X := Probe.X + Probe.Vx;
            if Probe.Vx > 0 then
               Probe.Vx := Probe.Vx - 1;
            end if; -- Probe.Vx > 0
            Success := Target.X1 <= Probe.X and Probe.x <= Target.X2;
         end loop; -- not Success and Probe.X < Target.X2
      end Simulate_X;

      procedure Simulate_Y (Target : in Targets;
                            Probe : in out Probes;
                            Vy : in Y_Velocities;
                            Success : out Boolean) is

      begin -- Simulate_Y
         Y_Max := Integer'First;
         Success := False;
         Probe.Y := 0;
         Probe.Vy := Vy;
         while not Success and Probe.Y > Target.Y2 loop
            Probe.Y := Probe.Y + Probe.Vy;
            Probe.Vy := Probe.Vy - 1;
            Success := Target.Y1 >= Probe.Y and Probe.Y >= Target.Y2;
         end loop; -- not Success and Probe.Y > Target.Y2
      end Simulate_Y;

      Success : Boolean;
      Y_Max_Test : Y_Coordinates;
      Vx_Store : Vx_Stores.Vector;
      Vy_Store : Vy_Stores.Vector;

   begin -- Optomise
      for Vx in X_Velocities range Vx_In .. Target.X2 loop
         -- N.B. The upper limit is reaching the target on the first step!
         Simulate_X (Target, Probe, Vx, Success);
         if Success then
            Append (Vx_Store, Vx);
         end if; -- Success
      end loop; -- Vx in X_Velocities range Vx_In .. Target.X2
      for Vy In Y_Velocities range Target.Y2 .. 10 * Vy_In loop
         -- N.B. The lower limit (most negative) is reaching the target on the
         -- first step! There may be mathematics to calculate an upper limit,
         -- limiting it as above is just a guess!
         Simulate_Y (Target, Probe, Vy, Success);
         if Success then
            Append (Vy_Store, Vy);
         end if; -- Success
      end loop; --  Vy In Y_Velocities range Target.Y2 .. 10 * Vy_In
      -- We now have a list of Vx and Vy values that step into the target zone;
      -- however the part two solution is not the product of the two mengths of
      -- the two lists not every pair works. By the above strategy we have
      -- reduced an N * M problem to the product of two relatively small
      -- subsets.
      Pair_Count := 0;
      for Xc In Iterate (Vx_Store) loop
         for Yc in Iterate (Vy_Store) loop
            Simulate (Target, Probe, Vx_Store (Xc), Vy_Store (Yc), Success,
                      Y_Max_Test);
            if Success then
               Pair_Count := Pair_Count + 1;
               if Y_Max_Test > Y_Max then
                  Y_Max := Y_Max_Test;
               end if; -- Y_Max_Test > Y_Max
            end if; -- Success
         end loop; -- Yc in Iterate (Vy_Store)
      end loop; -- Xc In Iterate (Vx_Store)
   end Optomise;

   Target : Targets;
   Probe : Probes;
   Y_Max : Y_Coordinates;
   Vx_Hit : X_Velocities;
   Vy_Hit : Y_Velocities;
   Pair_Count : Natural;

begin -- December_17
   Get_Input (Target);
   Find_Target (Target, Probe, Vx_Hit, Vy_Hit, Y_Max);
   Optomise (Target, Probe, Vx_Hit, Vy_Hit, Y_Max, Pair_Count);
   Put_Line ("Part One Answer:" & Y_Max'Img);
   Put_Line ("Part Two Answer:" & Pair_Count'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_17;
