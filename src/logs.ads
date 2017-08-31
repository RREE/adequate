with Alog;                         use Alog;
with Alog.Logger;

package Logs with Elaborate_Body is

   L : Logger.Instance (Init => False); -- no default output to stdout

end Logs;
