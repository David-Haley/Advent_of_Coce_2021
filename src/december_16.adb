with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Interfaces; use Interfaces;
with DJH.Execution_Time;

procedure December_16 is

   subtype Bit_Indices is Natural;

   Nibble_Size : constant Bit_Indices := 4;

   package Bit_Stores is new Ada.Containers.Vectors (Bit_Indices, Boolean);
   use Bit_Stores;

   type Bit_Streams is record
      Bit_Index : Bit_Indices := 0;
      Bit_Store : Bit_Stores.Vector;
      Last_Bit : Bit_Indices := 0;
   end record; -- Bit_Streams

   subtype Words is Unsigned_64;
   subtype Word_Lengths is Positive range 1 .. 63;

   package Word_IO is new Ada.Text_IO.Modular_IO (Words);
   use Word_IO;

   procedure Get_Input (Bit_Stream : out Bit_Streams) is

      -- december_16 [Input_File_Name]

      Bit_0_Mask : constant Unsigned_8 := 2#00000001#;

      Input_File : File_Type;
      Text : Unbounded_String;
      Nibble_Value : Unsigned_8;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_16.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Text);
      for N in Positive range 1 .. Length (Text) loop
         Nibble_Value :=
           Unsigned_8'Value ("16#" & Element (Text, N) & "#");
         for B in reverse Natural range 0 .. 3 loop
            Append (Bit_Stream.Bit_Store, (Nibble_Value and
                      Shift_Left (Bit_0_Mask, B)) > 0);
         end loop; -- B in reverse Natural range 0 .. 3
      end loop; -- N in Positive range 1 .. Length (Text)
      Bit_Stream.Last_Bit := Last_Index (Bit_Stream.Bit_Store);
      Close (Input_File);
   end Get_Input;

   procedure Parse (Bit_Stream : in out Bit_Streams;
                    Part_One, Part_Two : out Words) is

      package Results is new Ada.Containers.Vectors (Positive, Words);
      use Results;


      function Sub_Parse (Bit_Stream : in out Bit_Streams;
                          Part_One : in out Words;
                          Number_Of_Sub_Packets : Words := Words'Last)
                          return Results.Vector is

         -- Sub_Parse has side effects of modifying Bit_Stream and Part_One see
         -- above declaration

         function Pack (Bit_Stream : in out Bit_Streams;
                        Word_Length : in Word_Lengths) return Words is

            LSB : constant Words := 1;
            Result : Words := 0;

         begin -- Pack
            for I in Word_Lengths range 1 .. Word_Length loop
               Result := Shift_Left (Result, 1);
               if Bit_Stream.Bit_Store (Bit_Stream.Bit_Index) then
                  Result := Result or LSB;
               end if; -- Bit_Stream.Bit_Store (Bit_Stream.Bit_Index)
               Bit_Stream.Bit_Index := Bit_Stream.Bit_Index + 1;
            end loop; -- I in Word_Lengths range 1 .. Word_Length
            return Result;
         end Pack;

         function Get_Literal (Bit_Stream : in out Bit_Streams) return Words is

            Result : Words := 0;

         begin -- Get_Literal
            while Pack (Bit_Stream, 1) > 0 loop
               Result := Shift_Left (Result, Nibble_Size) or
                 Pack (Bit_Stream, Nibble_Size);
            end loop; -- Pack (Bit_Stream, 1) > 0
            -- Zero bit, one more Nibble to read
            Result := Shift_Left (Result, Nibble_Size) or
              Pack (Bit_Stream, Nibble_Size);
            return Result;
         end Get_Literal;

         function Transfer (Bit_Stream : in out Bit_Streams;
                            N : in Words) return Bit_Streams is

            Result : Bit_Streams;

         begin -- Transfer
            Result.Bit_Store := Bit_Stores.Empty_Vector;
            for I in Words range 1 .. N loop
               Append (Result.Bit_Store,
                       Bit_Stream.Bit_Store (Bit_Stream.Bit_Index));
               Bit_Stream.Bit_Index := Bit_Stream.Bit_Index + 1;
            end loop; -- I in Words range 1 .. N
            Result.Last_Bit := Last_Index (Result.Bit_Store);
            return Result;
         end Transfer;

         Function Get_Sub_Packet_Results (Bit_Stream : in out Bit_Streams)
                                          return Results.Vector is

            Length_Bits : constant Word_Lengths := 15;
            Count_Bits : constant Word_Lengths := 11;
            Sub_Packet_Length, Sub_Packet_Count : Words;
            Sub_Packet : Bit_Streams;
            Result : Results.Vector := Results.Empty_Vector;

         begin -- Get_Sub_Packet_Results
            if Pack (Bit_Stream, 1) = 1 then
               Sub_Packet_Count := Pack (Bit_Stream, Count_Bits);
               Result := Sub_Parse (Bit_Stream, Part_One, Sub_Packet_Count);
               Assert (Last_Index (Result) = Positive (Sub_Packet_Count),
                       "Expected" & Sub_Packet_Count'Img &
                         " results, found:" &
                         Results.Extended_Index'Image
                         (Last_Index (Result)));
            else
               Sub_Packet_Length := Pack (Bit_Stream, Length_Bits);
               Sub_Packet := Transfer (Bit_Stream, Sub_Packet_Length);
               Result := Sub_Parse (Sub_Packet, Part_One);
            end if; -- Pack (Bit_Stream, 1) = 1
            return Result;
         end Get_Sub_Packet_Results;

         Type_ID : Words;
         Result, Return_Result : Results.Vector;
         Single_Word : Words;
         Sub_Packet_Count : Words := 0;

      begin -- Sub_Parse
         while Bit_Stream.Bit_Index < Bit_Stream.Last_Bit - 6 and
           Sub_Packet_Count < Number_Of_Sub_Packets loop
            -- The header is at least six bits so if there are fewer than six
            -- bitsremaining, the end has been reached
            Part_One := Part_One + Pack (Bit_Stream, 3); -- Get version
            Type_ID := Pack (Bit_Stream, 3);
            Sub_Packet_Count := Sub_Packet_Count + 1;
            case Type_ID is
               when 0 => -- Sum
                  Single_Word := 0;
                  Return_Result := Get_Sub_Packet_Results (Bit_Stream);
                  for R in Iterate (Return_Result) loop
                     Single_Word := Single_Word + Return_Result (R);
                  end loop; -- R in Iterete (Return_Result)
                  Append (Result, Single_Word);
               when 1 => -- Product
                  Single_Word := 1;
                  Return_Result := Get_Sub_Packet_Results (Bit_Stream);
                  for R in Iterate (Return_Result) loop
                     Single_Word := Single_Word * Return_Result (R);
                  end loop; -- R in Iterete (Return_Result)
                  Append (Result, Single_Word);
               when 2 => -- Minimum
                  Single_Word := Words'Last;
                  Return_Result := Get_Sub_Packet_Results (Bit_Stream);
                  for R in Iterate (Return_Result) loop
                     if Single_Word > Return_Result (R) then
                        Single_Word := Return_Result (R);
                     end if; -- Single_Word > Return_Result
                  end loop; -- R in Iterete (Return_Result)
                  Append (Result, Single_Word);
               when 3 => -- Maximum
                  Single_Word := Words'First;
                  Return_Result := Get_Sub_Packet_Results (Bit_Stream);
                  for R in Iterate (Return_Result) loop
                     if Single_Word < Return_Result (R) then
                        Single_Word := Return_Result (R);
                     end if; -- Single_Word < Return_Result
                  end loop; -- R in Iterete (Return_Result)
                  Append (Result, Single_Word);
               when 4 =>
                  Append (Result, Get_Literal (Bit_Stream));
               when 5 => -- >
                  Return_Result := Get_Sub_Packet_Results (Bit_Stream);
                  Assert ( Length (Return_Result) = 2,
                           "Incorrect Number of Parameters for > :" &
                             Count_Type'Image (Length (Return_Result)));
                  if Return_Result (1) > Return_Result (2) then
                     Append (Result, 1);
                  else
                     Append (Result, 0);
                  end if; -- Return_Result (0) > Return_Result (1)
               when 6 => -- <
                  Return_Result := Get_Sub_Packet_Results (Bit_Stream);
                  Assert ( Length (Return_Result) = 2,
                           "Incorrect Number of Parameters for < :" &
                             Count_Type'Image (Length (Return_Result)));
                  if Return_Result (1) < Return_Result (2) then
                     Append (Result, 1);
                  else
                     Append (Result, 0);
                  end if; -- Return_Result (0) < Return_Result (1)
               when 7 => -- =
                  Return_Result := Get_Sub_Packet_Results (Bit_Stream);
                  Assert ( Length (Return_Result) = 2,
                           "Incorrect Number of Parameters for = :" &
                             Count_Type'Image (Length (Return_Result)));
                  if Element (Return_Result, 1) =
                    Element (Return_Result, 2) then
                     Append (Result, 1);
                  else
                     Append (Result, 0);
                  end if; -- Element (Return_Result, 1) = ...
               when others =>
                  Assert (False, "Unknown Type_ID:" & Type_ID'Img);
            end case;
         end loop; -- Bit_Stream.Bit_Index < Bit_Stream.Last_Bit - 6 and ...
         return Result;
      end Sub_Parse;

      Result : Results.Vector;

   begin -- Parse
      Part_One := 0;
      Result := Sub_Parse (Bit_Stream, Part_One);
      Part_Two := Result (1);
   end Parse;

   Bit_Stream :  Bit_Streams;
   Part_One, Part_Two : Words;

begin -- December_16
   Get_Input (Bit_Stream);
   Parse (Bit_Stream, Part_One, Part_Two);
   Put_Line ("Part One Answer:" & Part_One'Img);
   Put_Line ("Part Two Answer:" & Part_Two'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_16;
