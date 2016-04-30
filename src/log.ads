with Alog;                         use Alog;
with Alog.Logger;
with Alog.Policy_DB;
with Alog.Facilities.File_Descriptor;

package Log is
   L : Logger.Instance (Init => True);
end Log;
