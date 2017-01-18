--  Abstract :
--
--  Test SAL.Gen.Word_Order_Convert.
--
--  Copyright (C) 2001, 2002, 2006 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;
with SAL.Gen_Word_Order_Convert;
with SAL.Gen_Word_Order_Convert.Gen_Scalar_32;
with SAL.Gen_Word_Order_Convert.Gen_Scalar_64;
with SAL.Generic_Hex_Image;
procedure Test_Gen_Word_Order_Convert
is
   type Bus_Word_Type is mod 2 ** 16;
   type Bus_Index_Type is range -256 .. 255;
   type Bus_Word_Array_Type is array (Bus_Index_Type range <>) of Bus_Word_Type;

   type Host_Signed_Int_16_Type is range -2 ** 15 .. 2 ** 15 - 1;
   type Host_Unsigned_Int_16_Type is mod 2 ** 16;

   type Host_Signed_Int_32_Type is range -2 ** 31 .. 2 ** 31 - 1;
   type Host_Unsigned_Int_32_Type is mod 2 ** 32;

   type Host_Float_32_Type is digits 6;
   type Host_Float_64_Type is digits 12;

   type Host_Fixed_32_Type is delta 0.1 digits 9;
   for Host_Fixed_32_Type'Size use 32;

   package Bus_Order_Convert is new SAL.Gen_Word_Order_Convert
      (Bus_Word_Type       => Bus_Word_Type,
       Bus_Index_Type      => Bus_Index_Type,
       Bus_Word_Array_Type => Bus_Word_Array_Type);

   package Float_32_Convert is new Bus_Order_Convert.Gen_Scalar_32 (Host_Float_32_Type);
   package Float_64_Convert is new Bus_Order_Convert.Gen_Scalar_64 (Host_Float_64_Type);
   package Fixed_32_Convert is new Bus_Order_Convert.Gen_Scalar_32 (Host_Fixed_32_Type);

   function Hex_Image is new SAL.Generic_Hex_Image (4, Bus_Word_Type);

   use Bus_Order_Convert, Float_32_Convert, Float_64_Convert, Fixed_32_Convert;

   type Host_Data_Type is record
      Signed_16   : Host_Signed_Int_16_Type;
      Unsigned_16 : Host_Unsigned_Int_16_Type;
      Signed_32   : Host_Signed_Int_32_Type;
      Unsigned_32 : Host_Unsigned_Int_32_Type;
      Float_32    : Host_Float_32_Type;
      Float_64    : Host_Float_64_Type;
      Fixed_32    : Host_Fixed_32_Type;
   end record;

   Host_Data : constant Host_Data_Type :=
      (Signed_16   => 16#1234#,
       Unsigned_16 => 16#F123#,
       Signed_32   => 16#12345678#,
       Unsigned_32 => 16#F1234567#,
       Float_32    => 0.0,
       Float_64    => 1234567.8,
       Fixed_32    => 2.0);

   Read_Data : Host_Data_Type;

   Bus_Data      : Bus_Word_Array_Type (1 .. 32);
   Bus_Data_Last : Bus_Index_Type;
begin

   Bus_Data_Last := 0;
   To_Bus (Interfaces.Integer_16 (Host_Data.Signed_16), Bus_Data, Bus_Data_Last);
   To_Bus (Interfaces.Unsigned_16 (Host_Data.Unsigned_16), Bus_Data, Bus_Data_Last);
   To_Bus (Interfaces.Integer_32 (Host_Data.Signed_32), Bus_Data, Bus_Data_Last);
   To_Bus (Interfaces.Unsigned_32 (Host_Data.Unsigned_32), Bus_Data, Bus_Data_Last);
   To_Bus (Host_Data.Float_32, Bus_Data, Bus_Data_Last);
   To_Bus (Host_Data.Float_64, Bus_Data, Bus_Data_Last);
   To_Bus (Host_Data.Fixed_32, Bus_Data, Bus_Data_Last);

   Put ("Bus_Data => ");
   for I in 1 .. Bus_Data_Last loop
      Put (Hex_Image (Bus_Data (I)));
      Put (' ');
   end loop;
   New_Line;

   Bus_Data_Last := 0;
   From_Bus (Interfaces.Integer_16 (Read_Data.Signed_16), Bus_Data, Bus_Data_Last);
   From_Bus (Interfaces.Unsigned_16 (Read_Data.Unsigned_16), Bus_Data, Bus_Data_Last);
   From_Bus (Interfaces.Integer_32 (Read_Data.Signed_32), Bus_Data, Bus_Data_Last);
   From_Bus (Interfaces.Unsigned_32 (Read_Data.Unsigned_32), Bus_Data, Bus_Data_Last);
   From_Bus (Read_Data.Float_32, Bus_Data, Bus_Data_Last);
   From_Bus (Read_Data.Float_64, Bus_Data, Bus_Data_Last);
   From_Bus (Read_Data.Fixed_32, Bus_Data, Bus_Data_Last);

   if Read_Data /= Host_Data then
      Put_Line ("From_Bus failed");
   else
      Put_Line ("From_Bus passed");
   end if;
end Test_Gen_Word_Order_Convert;
