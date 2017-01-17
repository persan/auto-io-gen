--  Abstract:
--
--  Scalar types and operations, in addition to
--  Ada.Numerics.Generic_Elementary_Functions.
--
--  Copyright (C) 2001 - 2007 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
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

with Ada.Numerics.Generic_Elementary_Functions;
with Interfaces;
generic
   --  Auto_Io_Gen : ignore
   with package Elementary is new Ada.Numerics.Generic_Elementary_Functions (Real_Type);
package SAL.Gen_Math.Gen_Scalar is
   pragma Pure;

   e : constant := Ada.Numerics.e;

   function Modulo (Dividend, Divisor : in Real_Type) return Real_Type;
   function "mod" (Dividend, Divisor : in Real_Type) return Real_Type renames Modulo;
   --  Modulus operation for type Real_Type analogous to Mod for type
   --  INTEGER. Note the definition for Mod of negative numbers as
   --  illustrated by the ADA reference manual applies also to type
   --  Real_Type.
   --
   --  raises Constraint_Error if Dividend / Divisor does.

   function Dead_Band
      (Item        : in Real_Type;
       Lower_Limit : in Real_Type)
       return Real_Type;
   --  return Item dead-banded by |Lower_Limit|.
   --
   --                 output
   --                   |   /
   --               ____|__/_ input
   --                /  |
   --               /   |

   function Detent
      (Item        : in Real_Type;
       Dead_Band   : in Real_Type;
       Upper_Limit : in Real_Type)
       return Real_Type;
   --  Smooth, dead-banded version of Sign.
   --  Returns:
   --    0.0 for |Item| < |Dead_Band|,
   --  +-1.0 for |Item| > |Upper_Limit|,
   --  linear interpolation for |Dead_Band| < |Item| < |Upper_Limit|
   --
   --  Dead_Band, Upper_Limit are assumed >= 0.0.
   --
   --                 Detent
   --                   |   ____+1
   --               ____|__/_______ input
   --         -1  ___/  |
   --                   |

   -----------
   --  Limits operations

   --  Auto_Io_Gen : separate - Get checks High > Low
   type Limit_Type is private;
   --  Private to enforce High > Low

   function Image (Item : in Limit_Type) return String;

   function To_Limit (Low, High : in Real_Type) return Limit_Type;
   --  Raises Invalid_Limit if Low >= High.

   function High (Item : in Limit_Type) return Real_Type;
   function Low (Item : in Limit_Type) return Real_Type;
   pragma Inline (High, Low);

   function "and" (Left, Right : in Limit_Type) return Limit_Type;
   --  Return the intersection of Left and Right, if it is non-null.
   --
   --  Raises Invalid_Limit if there is no intersection; for example if
   --  Left.High < Right.Low

   procedure Clip
      (Item    : in out Real_Type;
       Limit   : in     Limit_Type;
       Clipped :    out Boolean);
   --  Item is clipped so that Limit.Low <= Item <= Limit.High. Clipped is
   --  TRUE if Item is changed, FALSE otherwise.

   function "<="
      (Item   : in     Real_Type;
       Limit  : in     Limit_Type)
       return Boolean;
   --  TRUE if Limit.Low <= Item <= Limit.High, FALSE otherwise.

   type Scale_Limit_Type is record
      Scale  : Real_Type;
      Offset : Real_Type;
      Limit  : Limit_Type;
   end record;

   procedure Scale_Limit
     (Item    : in out Real_Type;
      Param   : in     Scale_Limit_Type;
      Result  :    out Real_Type;
      Clipped :    out Boolean);
   --  Apply Param to Item. First, if Item > Param.Limit.High, Clipped
   --  is True and Item is set to Param.Limit.High. Similarly for
   --  Param.Limit.Low. Then, Result := Item * Scale + Offset.

   function Compute_Limit
     (Scale   : in Real_Type;
      Offset  : in Real_Type;
      Default : in Scale_Limit_Type)
     return Limit_Type;
   --  Assuming Default.Limit is correct for Default.Scale,
   --  Default.Offset, compute new limits that will be correct for
   --  Scale and Offset, when used with Scale_Limit.

   procedure Clip_Scale_Limit
     (Scale_Limit : in out Scale_Limit_Type;
      Safe_Result : in     Limit_Type;
      Clipped     :    out Boolean);
   --  Adjust Scale_Limit.Limit so that Scale_Limit (Item,
   --  Scale_Limit, ..) will result in Item <= Safe_Result. Clipped is
   --  True if Scale_Limit changed.

   --  These values guarantee Integer_16 (value) or Unsigned_16
   --  (value) won't raise Constraint_Error, and allow for maximum
   --  round-off error. Useful with Clip_Scale_Limit when result will
   --  be converted to Integer_16 or Unsigned_16.
   Integer_16_First_Real : constant Real_Type := Real_Type (Interfaces.Integer_16'First) - 0.5 +
     (Real_Type'Epsilon * Real_Type (Interfaces.Integer_16'Last));

   Integer_16_Last_Real  : constant Real_Type := Real_Type (Interfaces.Integer_16'Last) + 0.5 -
     (Real_Type'Epsilon * Real_Type (Interfaces.Integer_16'Last));

   Unsigned_16_First_Real : constant Real_Type := -0.5 + Real_Type'Epsilon;
   Unsigned_16_Last_Real  : constant Real_Type := Real_Type (Interfaces.Unsigned_16'Last) + 0.5 -
     (Real_Type'Epsilon * Real_Type (Interfaces.Unsigned_16'Last));

   --------------
   --  Trig operations

   Pi      : constant := Ada.Numerics.Pi;
   Two_Pi  : constant := 2.0 * Ada.Numerics.Pi;
   Half_Pi : constant := 0.5 * Ada.Numerics.Pi;

   function First_Order_Trig return Real_Type;
   pragma Inline (First_Order_Trig);
   --  = Sqrt (Real_Type'Epsilon)
   --  A linear approximation is used for some operations when Trig.Sin < First_Order_Trig.

   --  Auto_Io_Gen : separate - Put, Get as single radians value
   type Trig_Pair_Type is private;
   --  Sin and Cos of an angle. Private to enforce sin**2 + cos**2 = 1.

   function Sin_Cos (Angle : in Real_Type) return Trig_Pair_Type;
   --  Compute Sin and Cos - possibly faster than separately

   function Atan2 (Trig : in Trig_Pair_Type) return Real_Type;
   pragma Inline (Atan2);
   --  Return the angle in radians, range - pi < angle <= + pi.
   --
   --  Raises Ada.Numerics.Argument_Error if Trig has both elements 0.0, which can only happen
   --  thru abuse of Unchecked_Trig_Pair.

   function Sin (Trig : in Trig_Pair_Type) return Real_Type;
   function Cos (Trig : in Trig_Pair_Type) return Real_Type;
   pragma Inline (Sin, Cos);
   --  Return the indicated part

   function To_Trig_Pair (Sin, Cos : in Real_Type) return Trig_Pair_Type;
   --  Normalize so that Sin ** 2 + Cos ** 2 = 1.
   --
   --  Raises Non_Normalizable_Trig_Pair if Sin ** 2 + Cos ** 2 = 0.0

   function Unchecked_Trig_Pair (Sin, Cos : in Real_Type) return Trig_Pair_Type;
   --  Performs no normalization; for use in situations where the
   --  algorithm guarantees trig identity.

   pragma Inline (Unchecked_Trig_Pair);

   function "+" (Left : in Trig_Pair_Type; Right : in Trig_Pair_Type) return Trig_Pair_Type;
   --  Find Sin_Cos {Left_Angle + Right_Angle} from Sin_Cos {Left_Angle}, Sin_Cos {Right_Angle}

   function "-" (Left : in Trig_Pair_Type; Right : in Trig_Pair_Type) return Trig_Pair_Type;
   --  Find Sin_Cos {Left_Angle - Right_Angle} from Sin_Cos {Left_Angle}, Sin_Cos {Right_Angle}

   function Half_Trig (Trig : in Trig_Pair_Type) return Trig_Pair_Type;
   --  Find Cos{angle/2}, Sin{angle/2} from Sin{angle}, Cos{angle}

   function Double_Trig (Trig : in Trig_Pair_Type) return Trig_Pair_Type;
   --  Find Cos{2*angle}, Sin{2*angle} from Sin{angle}, Cos{angle}

private

   type Limit_Type is record
      Low  : Real_Type;
      High : Real_Type;
   end record;

   type Trig_Pair_Type is
   record
      Sin : Real_Type;
      Cos : Real_Type;
   end record;

end SAL.Gen_Math.Gen_Scalar;
