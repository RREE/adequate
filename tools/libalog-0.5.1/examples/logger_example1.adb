with Alog.Logger;
with Alog.Facilities.File_Descriptor;

use Alog;

--  Alog logger example.
procedure Logger_Example1 is
   --  Initialize logger instance with default file descriptor facility
   --  (logs to stdout).
   Log : Logger.Instance (Init => True);
begin
   --  Write a message with loglevel 'Info' to stdout.
   Log.Log_Message
     (Level => Info,
      Msg   => "This is a testmessage from Alog logger");

   Attach_FD_Facility :
   declare
      FD : constant Facilities.File_Descriptor.Handle :=
        new Facilities.File_Descriptor.Instance;
   begin
      FD.Set_Logfile (Path => "/tmp/alog.log");
      Log.Attach_Facility (Facility => Facilities.Handle (FD));

      --  Log a message to file and stdout.
      Log.Log_Message (Source => "Example",
                       Level  => Warning,
                       Msg    => "Another testmessage");
   end Attach_FD_Facility;
end Logger_Example1;
