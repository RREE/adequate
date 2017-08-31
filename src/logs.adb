with Alog;                         use Alog;
with Alog.Facilities;
with Alog.Facilities.Syslog;

package body Logs is
   use Alog.Facilities.Syslog;

   Sys : constant Facilities.Syslog.Handle :=
     new Facilities.Syslog.Instance;
begin
   Sys.Set_Origin (LOG_DAEMON);
   Sys.Toggle_Write_Timestamp (False);
   L.Attach_Facility (Facilities.Handle (Sys));
   -- L.Attach_Default_Facility;
end Logs;
