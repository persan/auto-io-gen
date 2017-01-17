--  Abstract :
--
--  see spec
--
--  Copyright (C) 2008 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

with Ada.Numerics.Float_Random;
procedure SAL.Poly.Lists.Double.Gen_Randomize (List : in out List_Type)
is
   Source_Count : Integer := 0;
   Result       : List_Type;
   I            : Iterator_Type := First (List);
   I_Temp       : Iterator_Type;

   Generator : Ada.Numerics.Float_Random.Generator;

   procedure Random_Next (I : in out Iterator_Type)
   is
      Step : constant Integer := Integer (Float (Source_Count) * Ada.Numerics.Float_Random.Random (Generator));
   begin
      for J in 1 .. Step loop
         Next (I);
         if Is_Null (I) then
            I := First (List);
         end if;
      end loop;
   end Random_Next;

begin
   loop
      exit when I = null;
      Next (I);
      Source_Count := Source_Count + 1;
   end loop;

   I := First (List);

   loop
      I_Temp := I;

      Next (I);

      Splice_After
        (Source => List,
         First  => I_Temp,
         Last   => I_Temp,
         Dest   => Result,
         After  => Last (Result));

      Source_Count := Source_Count - 1;

      exit when Is_Null (I);

      Random_Next (I);
   end loop;

   Splice_After (Result, First (Result), Last (Result), List, null);

end SAL.Poly.Lists.Double.Gen_Randomize;
