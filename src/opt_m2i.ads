--  command line options for mqtt2influx
with GNAT.Strings;                 use GNAT.Strings;

package Opt_M2I is

   --  mqtt2influx is normally run in daemon mode. That means it must
   --  be started as root and then converts to the specified user. If
   --  in daemon mode all output is sent to syslog. If not in daemon
   --  mode the output is sent to standard output.
   --
   --  In daemon mode the configuration is read from
   --  /etc/mqtt2influx.cfg or from the indicated file Config_File.
   --  In non-daemon mode the config file is searched in the local
   --  directory.

   Daemon          : aliased Boolean;
   User            : aliased String_Access;
   Config_File     : aliased String_Access;
   Verbosity_Level : aliased Integer;
   --
   procedure Set_Options;
end Opt_M2I;
