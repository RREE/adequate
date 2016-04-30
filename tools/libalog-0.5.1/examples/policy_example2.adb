with Alog.Policy_DB;
with Alog.Logger;
with Alog.Facilities.File_Descriptor;

use Alog;

--  Alog destination loglevel policy example.
procedure Policy_Example2 is
   Log    : Logger.Instance (Init => True);
   Errors : constant Facilities.File_Descriptor.Handle :=
     new Facilities.File_Descriptor.Instance;
begin
   --  Write all error messages to '/tmp/errors.log'.
   Errors.Set_Logfile (Path => "/tmp/errors.log");
   Errors.Set_Name (Name => "Application_Errors");
   Errors.Toggle_Write_Loglevel (State => True);

   --  Set loglevel policy to 'Error' for destination 'Application_Errors'.
   Policy_DB.Set_Loglevel (Identifier => "Application_Errors",
                           Level      => Error);

   Log.Attach_Facility (Facility => Facilities.Handle (Errors));

   --  This message will appear on stdout, but not in the error logfile.
   Log.Log_Message (Level  => Info,
                    Msg    => "This is not an error");
   --  This message will also be written to the error logfile.
   Log.Log_Message (Level  => Error,
                    Msg    => "This is an error");
end Policy_Example2;
