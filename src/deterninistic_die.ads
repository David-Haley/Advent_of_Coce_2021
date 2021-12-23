package Deterninistic_Die is

   subtype Die_Values is Positive range 1 .. 100;

   procedure Reset_Die;

   function Roll_Die return Die_Values;

   Function Die_Throws return Natural;

end Deterninistic_Die;
