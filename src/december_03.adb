with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

procedure December_03 is

   subtype Diagnostic_Datum is Unsigned_32;
   -- Unsugned_16 could be used; however the product of two Unsigned_16 can
   -- exceed Unsigned_16'Last. The alternative would be to convert to
   -- Unsigned_32 before calculating the product.

   package Diag_Stores is new
     Ada.Containers.Vectors (Positive, Diagnostic_Datum);
   use Diag_Stores;

   subtype Bit_Indices is Natural range 0 .. 31;
   -- Should match the bit numbers in Diagnostic_Datum

   Bit_Mask : constant Diagnostic_Datum := 1;

   function Filter (Diag_Store : in Diag_Stores.Vector;
                    Bit : in Bit_Indices;
                    Oxigen : in Boolean) return Diag_Stores.Vector is

      One_Count, Zero_Count : Natural := 0;
      Mask : constant Diagnostic_Datum := Shift_Left (Bit_Mask, Bit);
      Selector : Diagnostic_Datum;
      Result : Diag_Stores.Vector;

   begin -- Filter
      for I in Iterate (Diag_Store) loop
         if (Diag_Store (I) and Mask) /= 0 then
            One_Count := One_Count + 1;
         else
            Zero_Count := Zero_Count + 1;
         end if; --  (Diag_Store (I) and Mask) /= 0
      end loop; -- I in Iterate (Diag_Store)
      if Oxigen then
         if One_Count >= Zero_Count then
            Selector := Mask;
         else
            Selector := 0;
         end if; -- One_Count >= Zero_Count
      else
         if Zero_Count <= One_Count then
            Selector := 0;
         else
            Selector := Mask;
         end if; -- Zero_Count < One_Count
      end if; -- Oxigen
      for I in Iterate (Diag_Store) loop
         if (Diag_Store (I) and Mask) = Selector then
            Append (Result, Diag_Store (I));
         end if; -- (Diag_Store (I) and Mask) = Selector
      end loop; -- I in Iterate (Diag_Store)
      return Result;
   end Filter;

   Input_File : File_Type;
   Text : Unbounded_String;
   Diag_Store, Oxigen_Store, CO2_Store : Diag_Stores.Vector;
   Bit_Count : array (Bit_Indices) of Natural := (others => 0);
   Epsilon, Gamma : Diagnostic_Datum := 0;
   MSB, Current_Bit : Bit_Indices;

begin -- December_03
   if Argument_Count = 0 then
      Open (Input_File, In_File, "december_03.txt");
   else
      Open (Input_File, In_File, Argument(1));
   end if; -- Argument_Count = 0
   while not End_Of_Line (Input_File) loop
      Get_Line (Input_File, Text);
      if Length (Diag_Store) = 0 then
         MSB := Length (Text) - 1;
         -- This is a little rough, it will fail if there are leading or
         -- trailing spaces. A more correct solution would be to find the
         -- length of the token containing only binary digits.
      elsif MSB /= Length (Text) - 1 then
         Put_Line ("Unexpected variable word length");
      end if; -- Length (Diag_Store) = 0
      Append (Diag_Store,
              Diagnostic_Datum'Value ("2#" & To_String (Text) & '#'));
   end loop; -- not End_Of_Line (Input_File)
   Close (Input_File);
   for I in iterate (Diag_Store) loop
      for B in Bit_Indices range 0 .. MSB loop
         if (Diag_Store (I) and Shift_Left (Bit_Mask, B)) > 0 then
            Bit_Count (B) := Bit_Count (B) + 1;
         end if; -- (Diag_Store (I) and Shift_Left (Bit_Mask, B)) > 0
      end loop; -- B in Bit_Indices range 0 .. MSB
   end loop; -- I in iterate (Diag_Store)
   for B in Bit_Indices range 0 .. MSB loop
      if Bit_Count (B) > Natural (Length (Diag_Store)) / 2 then
         Gamma := Gamma or Shift_Left (Bit_Mask, B);
      else
         Epsilon := Epsilon or Shift_Left (Bit_Mask, B);
      end if; -- Bit_Count (B) > Natural (Length (Diag_Store)) / 2
   end loop; -- Bit_Indices range 0 .. MSB
   Put_Line ("Part One Answer:" & Diagnostic_Datum'Image (Epsilon * Gamma));
   Current_Bit := MSB;
   Oxigen_Store := Copy (Diag_Store);
   loop -- Fiter Oxigen Diagnostics
      Oxigen_Store := Filter (Oxigen_Store, Current_Bit, True);
      exit when Length (Oxigen_Store) = 1 or Current_Bit = 0;
      Current_Bit := Current_Bit - 1;
   end loop; -- Fiter Oxigen Diagnostics
   Current_Bit := MSB;
   CO2_Store := Copy (Diag_Store);
   loop -- Fiter CO2 Diagnostics
      CO2_Store := Filter (CO2_Store, Current_Bit, False);
      exit when Length (CO2_Store) = 1 or Current_Bit = 0;
      Current_Bit := Current_Bit - 1;
   end loop; -- Fiter CO2 Diagnostics
   Put_Line ("Part Two Answer:" &
               Diagnostic_Datum'Image (Oxigen_Store (1) * CO2_Store (1)));
end December_03;
