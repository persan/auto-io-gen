--  Example from http://www.ada-auth.org/ai-files/grab_bag/bitorder.pdf
--  This demonstrates that GNAT 5.03a does not implement AI95-00133
--  Also see Ada Letters September 2005, article by Randal Andress.

with SAL.Interfaces_More; use SAL.Interfaces_More;
with System;
procedure Endianness_Example
is
use System;

   --  This fails with GNAT if Bit_Order is not the default
   type Other_Date_And_Time_Type is record
      Years_Since_1980 : Unsigned_7;
      Month            : Unsigned_4;
      Day_Of_Month     : Unsigned_5;
      Hour             : Unsigned_5;
      Minute           : Unsigned_6;
      Seconds          : Unsigned_5;
   end record;

   for Other_Date_And_Time_Type'Bit_Order use System.High_Order_First;
   for Other_Date_And_Time_Type use record
      Years_Since_1980 at 0 range 0 .. 6;
      Month            at 0 range 7 .. 10;
      Day_Of_Month     at 0 range 11 .. 15;
      Hour             at 2 range 0 .. 4;
      Minute           at 2 range 5 .. 10;
      Seconds          at 2 range 11 .. 15;
   end record;

   --  This fails with GNAT if Bit_Order is not the default
   type Date_And_Time_Type is record
      Years_Since_1980 : Unsigned_7;
      Month            : Unsigned_4;
      Day_Of_Month     : Unsigned_5;
      Hour             : Unsigned_5;
      Minute           : Unsigned_6;
      Seconds          : Unsigned_5;
   end record;
   for Date_And_Time_Type'Bit_Order use System.Low_Order_First;
   for Date_And_Time_Type use record
      Years_Since_1980 at 0 range 9 .. 15;
      Month            at 0 range 5 .. 8;
      Day_Of_Month     at 0 range 0 .. 4;
      Hour             at 2 range 11 .. 15;
      Minute           at 2 range 5 .. 10;
      Seconds          at 2 range 0 .. 4;
   end record;

   --  This works for either target bit order
   type Good_Date_And_Time_Type is record
      Years_Since_1980 : Unsigned_7;
      Month            : Unsigned_4;
      Day_Of_Month     : Unsigned_5;
      Hour             : Unsigned_5;
      Minute           : Unsigned_6;
      Seconds          : Unsigned_5;
   end record;
   for Good_Date_And_Time_Type use record
      Years_Since_1980 at 0 range
        Low_Bit_First  * (LSBit_16 + Bit_Order * 9) + High_Bit_First * (LSBit_32 + Bit_Order * 15) ..
        High_Bit_First * (LSBit_16 + Bit_Order * 9) + Low_Bit_First  * (LSBit_32 + Bit_Order * 15);
      Month            at 0 range
        Low_Bit_First  * (LSBit_16 + Bit_Order * 5) + High_Bit_First * (LSBit_32 + Bit_Order * 8) ..
        High_Bit_First * (LSBit_16 + Bit_Order * 5) + Low_Bit_First  * (LSBit_32 + Bit_Order * 8);
      Day_Of_Month     at 0 range
        Low_Bit_First  * (LSBit_16 + Bit_Order * 0) + High_Bit_First * (LSBit_32 + Bit_Order * 4) ..
        High_Bit_First * (LSBit_16 + Bit_Order * 0) + Low_Bit_First  * (LSBit_32 + Bit_Order * 4);
      Hour             at 2 range
        Low_Bit_First  * (LSBit_16 + Bit_Order * 11) + High_Bit_First * (LSBit_32 + Bit_Order * 15) ..
        High_Bit_First * (LSBit_16 + Bit_Order * 11) + Low_Bit_First  * (LSBit_32 + Bit_Order * 15);
      Minute           at 2 range
        Low_Bit_First  * (LSBit_16 + Bit_Order * 5) + High_Bit_First * (LSBit_32 + Bit_Order * 10) ..
        High_Bit_First * (LSBit_16 + Bit_Order * 5) + Low_Bit_First  * (LSBit_32 + Bit_Order * 10);
      Seconds          at 2 range
        Low_Bit_First  * (LSBit_16 + Bit_Order * 0) + High_Bit_First * (LSBit_32 + Bit_Order * 4) ..
        High_Bit_First * (LSBit_16 + Bit_Order * 0) + Low_Bit_First  * (LSBit_32 + Bit_Order * 4);
   end record;

begin
   null;
end Endianness_Example;
