with Alog;                         use Alog;
with Alog.Logger;

package Logs with Elaborate_Body is

   L : Logger.Instance (Init => False); -- no default output to stdout

   procedure Set_Log_Dest_To (Dest : String);

end Logs;
