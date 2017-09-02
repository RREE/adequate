with Alog;                         use Alog;
with Alog.Facilities;
with Alog.Facilities.Syslog;
with Alog.Facilities.File_Descriptor;
with Opt_M2I;

package body Logs is
   use Alog.Facilities.Syslog;

   H : Facilities.Handle;

   procedure Set_Log_Dest_To (Dest : String)
   is
   begin
      if Dest = "syslog" then
         H := new Facilities.Syslog.Instance;
         Facilities.Syslog.Handle(H).Set_Origin (LOG_DAEMON);

         -- Sys.Set_Origin (LOG_DAEMON);

      elsif Dest = "stdout" then
         H := new Facilities.File_Descriptor.Instance;
         Facilities.File_Descriptor.Handle(H).Set_Logfile_Standard_Output;

      elsif Dest = "stderr" then
         H := new Facilities.File_Descriptor.Instance;
         Facilities.File_Descriptor.Handle(H).Set_Logfile_Standard_Error;

      elsif Dest(Dest'First) ='/' then
         H := new Facilities.File_Descriptor.Instance;
         --  Use Dest as logfile, overwrite existing file.
         Facilities.File_Descriptor.Handle(H).Set_Logfile (Dest, Append => False);
      else
         raise Opt_M2I.Stop_Failure with "don't know how to log to '" & Dest & "'.";
      end if;

      --  Enable writing of loglevels.
      H.Toggle_Write_Loglevel (True);
      H.Toggle_Write_Timestamp (False);
      L.Attach_Facility (H);
   end Set_Log_Dest_To;

begin
   Set_Log_Dest_To ("stderr");
end Logs;
