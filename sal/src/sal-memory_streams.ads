with System.Address_To_Access_Conversions;
with Ada.Streams; use Ada.Streams;
package SAL.Memory_Streams is
   pragma Preelaborate; -- System.Address_To_Access_Conversions is.

   --  We can't use IO_Exceptions, because that package isn't Pure.
   Status_Error : exception;
   End_Error    : exception;

private
   --  needed by child packages
   package Stream_Element_Address_Conversions is new System.Address_To_Access_Conversions (Stream_Element);

   type Direction_Type is (In_Stream, Out_Stream);

end SAL.Memory_Streams;
