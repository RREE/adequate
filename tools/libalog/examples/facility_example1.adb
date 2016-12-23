with Alog.Log_Request;
with Alog.Facilities.File_Descriptor;

use Alog;

--  Alog file descriptor facility example.
procedure Facility_Example1 is
   Facility : Facilities.File_Descriptor.Instance;
begin
   --  Enable writing of loglevels.
   Facility.Toggle_Write_Loglevel (State => True);

   --  Use '/tmp/alog.log' as logfile, overwrite existing file.
   Facility.Set_Logfile (Path   => "/tmp/alog.log",
                         Append => False);

   --  Let the facility process a log request with loglevel 'Warning'.
   Facility.Process
     (Request => Log_Request.Create
        (Level   => Warning,
         Message => "This is a testmessage from Alog FD facility"));

   --  Teardown the facility.
   Facility.Teardown;
end Facility_Example1;
