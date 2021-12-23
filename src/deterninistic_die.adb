package body Deterninistic_Die is

   Die_Thrown : Integer := 0;

   procedure Reset_Die is

   begin -- Reset_Die
      Die_Thrown := 0;
   end Reset_Die;

   function Roll_Die return Die_Values is

      Result : Die_Values := Die_Thrown mod Die_Values'Last + 1;

   begin -- Roll_Die
      Die_Thrown := Die_Thrown + 1;
      return Result;
   end Roll_Die;

   Function Die_Throws return Natural is

   begin -- Die_Throws
      return Die_Thrown;
   end Die_Throws;

end Deterninistic_Die;
