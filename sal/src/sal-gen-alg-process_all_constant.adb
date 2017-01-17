--  Abstract :
--
--  See spec
--
--  Copyright (C) 1999, 2000 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce an
--  executable, that does not by itself cause the resulting executable
--  to be covered by the GNU General Public License. This exception
--  does not however invalidate any other reasons why the executable
--  file might be covered by the GNU Public License.

procedure SAL.Gen.Alg.Process_All_Constant (Container : in Container_Type)
is
   I          : Iterator_Type := First (Container);
   First_Item : Boolean       := True;
begin
   Pre_Process_Container (Container);
   loop
      exit when Is_Null (I);
      Process_Item (Current (I), First_Item);
      First_Item := False;
      Next_Procedure (I);
   end loop;
   Post_Process_Container (Container);
end SAL.Gen.Alg.Process_All_Constant;
