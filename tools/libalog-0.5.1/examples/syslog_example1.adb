with Alog.Logger;
with Alog.Facilities.Syslog;

use Alog;

--  Alog syslog example.
procedure Syslog_Example1 is
   Log    : Logger.Instance (Init => False);
   Syslog : constant Facilities.Syslog.Handle :=
     new Facilities.Syslog.Instance;
begin
   Log.Attach_Facility (Facility => Facilities.Handle (Syslog));

   Syslog.Set_Origin (Value => Facilities.Syslog.LOG_DAEMON);
   Log.Log_Message (Level  => Debug,
                    Msg    => "This is a testmessage");
end Syslog_Example1;
