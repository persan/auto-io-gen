--  Abstract :
--
--  Utilities for capturing stack traces.
--
--  We don't just use GNAT.Traceback.Symbolic, because that isn't
--  supported on Lynx 4.0 as of GNAT 6.2.1.
--
--  Notice
--
--  Copyright (C) 2004, 2006, 2009 National Aeronautics and Space Administration.
--  No copyright is claimed in the United States under Title 17, U.S.
--  Code. All Foreign Rights are Reserved to the U.S. Government.
--
--  Disclaimer
--
--  This software is provided "as is" without any warranty of any,
--  kind either express, implied, or statutory, including, but not
--  limited to, any warranty that the software will conform to,
--  specifications any implied warranties of merchantability, fitness
--  for a particular purpose, and freedom from infringement, and any
--  warranty that the documentation will conform to the program, or
--  any warranty that the software will be error free.
--
--  In no event shall NASA be liable for any damages, including, but
--  not limited to direct, indirect, special or consequential damages,
--  arising out of, resulting from, or in any way connected with the
--  software or its documentation.  Whether or not based upon warranty,
--  contract, tort or otherwise, and whether or not loss was sustained
--  from, or arose out of the results of, or use of, the software,
--  documentation or services provided hereunder.
--
--  Export Control
--
--  The recipient of this software from NASA shall not export or
--  re-export directly or indirectly (including via remote access,
--  i.e. Internet) any part of this software or its documentation to
--  any country for which a validated license is required for such
--  export or re-export under the EXPORT LAWS without first obtaining
--  such a validated license.

pragma License (Unrestricted);

with Ada.Exceptions;
package SAL.Traceback is

   function Image (E : in Ada.Exceptions.Exception_Occurrence) return String;
   --  Return a stack traceback image suitable for output with
   --  Ada.Text_IO.Put_Line, then passing to addr2line.

end SAL.Traceback;
