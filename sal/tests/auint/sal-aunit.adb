--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004 - 2008 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
package body SAL.AUnit is

   procedure Gen_Check_Access
       (Label    : in String;
        Computed : in Item_Access_Type;
        Expected : in Item_Access_Type)
   is begin
      Standard.AUnit.Assertions.Assert (Computed = Expected, Label & " access type mismatch");
   end Gen_Check_Access;

   procedure Gen_Check_Discrete
       (Label    : in String;
        Computed : in Item_Type;
        Expected : in Item_Type)
   is begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & " got " & Item_Type'Image (Computed) & " expecting " & Item_Type'Image (Expected));
   end Gen_Check_Discrete;

   procedure Gen_Check_Array
     (Label    : in String;
      Computed : in Array_Type;
      Expected : in Array_Type)
   is begin
      Check_Index (Label & "'First", Computed'First, Expected'First);
      Check_Index (Label & "'Last", Computed'Last, Expected'Last);
      for I in Computed'Range loop
         Check_Item (Label & "." & Index_Type'Image (I), Computed (I), Expected (I));
      end loop;
   end Gen_Check_Array;

   procedure Gen_Check_Array_Tolerance
     (Label     : in String;
      Computed  : in Array_Type;
      Expected  : in Array_Type;
      Tolerance : in Tolerance_Type := Default_Tolerance)
   is begin
      Check_Index (Label & "'First", Computed'First, Expected'First);
      Check_Index (Label & "'Last", Computed'Last, Expected'Last);
      for I in Computed'Range loop
         Check_Item (Label & "." & Index_Type'Image (I), Computed (I), Expected (I), Tolerance);
      end loop;
   end Gen_Check_Array_Tolerance;

   procedure Gen_Check_Unconstrained_Array
     (Label          : in String;
      Computed       : in Array_Type;
      Expected       : in Array_Type;
      Strict_Indices : in Boolean    := True)
   is
      J : Index_Type := Expected'First;
   begin
      if Strict_Indices then
         Check_Index (Label & "'First", Computed'First, Expected'First);
         Check_Index (Label & "'Last", Computed'Last, Expected'Last);
      else
         Check (Label & "'Length", Computed'Length, Expected'Length);
      end if;

      for I in Computed'Range loop
         Check_Item (Label & "." & Index_Type'Image (I), Computed (I), Expected (J));
         if J /= Index_Type'Last then
            J := Index_Type'Succ (J);
         end if;
      end loop;
   end Gen_Check_Unconstrained_Array;

   procedure Gen_Check_Unconstrained_Array_Tolerance
     (Label          : in String;
      Computed       : in Array_Type;
      Expected       : in Array_Type;
      Tolerance      : in Item_Type  := Zero_Item;
      Strict_Indices : in Boolean    := True)
   is
      J : Index_Type := Expected'First;
   begin
      if Strict_Indices then
         Check_Index (Label & "'First", Computed'First, Expected'First);
         Check_Index (Label & "'Last", Computed'Last, Expected'Last);
      else
         Check (Label & "'Length", Computed'Length, Expected'Length);
      end if;

      for I in Computed'Range loop
         Check_Item (Label & "." & Index_Type'Image (I), Computed (I), Expected (J), Tolerance);
         if J /= Index_Type'Last then
            J := Index_Type'Succ (J);
         end if;
      end loop;
   end Gen_Check_Unconstrained_Array_Tolerance;

   procedure Gen_Check_Fixed
     (Label     : in String;
      Computed  : in Item_Type;
      Expected  : in Item_Type;
      Tolerance : in Item_Type := 0.0)
   is
      --  Can't use abs (Computed - Expected), since "abs 'first" should raise Constraint_Error
      Temp : constant Item_Type'Base := Computed - Expected;
   begin
      if Temp >= 0.0 then
         Standard.AUnit.Assertions.Assert
           (Temp <= abs Tolerance,
            Label & " got " & Item_Type'Image (Computed) & " expecting " & Item_Type'Image (Expected));
      else
         Standard.AUnit.Assertions.Assert
           (Temp >= -abs Tolerance,
            Label & " got " & Item_Type'Image (Computed) & " expecting " & Item_Type'Image (Expected));
      end if;
   end Gen_Check_Fixed;

   procedure Check
     (Label    : in String;
      Computed : in Integer;
      Expected : in Integer)
   is begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & " got " & Integer'Image (Computed) & " expecting " & Integer'Image (Expected));
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Boolean;
      Expected : in Boolean)
   is begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & " got " & Boolean'Image (Computed) & " expecting " & Boolean'Image (Expected));
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in String;
      Expected : in String)
   is begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & ASCII.LF &
           "got       '" & Computed & "'" & ASCII.LF &
           "expecting '" & Expected & "'");
   end Check;

   procedure Check
     (Label     : in String;
      Computed  : in Duration;
      Expected  : in Duration;
      Tolerance : in Duration := 0.0)
   is begin
      Standard.AUnit.Assertions.Assert
        (abs (Computed - Expected) <= Tolerance,
         Label & " got " & Duration'Image (Computed) & " expecting " & Duration'Image (Expected));
   end Check;

end SAL.AUnit;
