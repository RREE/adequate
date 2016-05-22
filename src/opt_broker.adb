--  options for aq_pub

with Ada.Command_Line;             use Ada.Command_Line;
with GNAT.Command_Line;            use GNAT.Command_Line;
with GNAT.Exception_Traces;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Exceptions;               use Ada.Exceptions;
with ALog;                         use ALog;
with Alog.Policy_DB;               use Alog.Policy_DB;
with Log;                          use Log;

package body Opt_Broker is

   Cfg : Command_Line_Configuration;
   Stop : exception;

   procedure Set_Cli_Options
   is
   begin
      -- GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
      Define_Switch (Cfg, Port_Int'Access, "-p:",
                     Long_Switch => "--port=",
                     Initial     => 1883,
                     Default     => 1883,
                     Argument    => "INT",
                     Help        => "MQTT server port (default: 1883).");

      Define_Switch (Cfg, Sys_Interval'Access, "-i:",
                     Long_Switch => "--sys_interval=",
                     Initial     => 10,
                     Default     => 10,
                     Argument    => "INT",
                     Help        => "number of seconds between publishing internal data");

      Define_Switch (Cfg, Verbosity_Level'Access, "-v?",
                     Long_Switch => "--verbose?",
                     Initial     => 1,
                     Default     => 2,
                     Argument    => "INT",
                     Help        => "Verbosity level: 0: quiet, 1: normal, 2: extended, 3: debug");

      Define_Switch (Cfg, "-h", Long_Switch => "--help", Help => "show this text");

      Getopt (Cfg);


      --
      -- check valid option ranges and convert to the target type
      --
      Port := GNAT.Sockets.Port_Type (Port_Int);

      if Sys_Interval < 0 then
         Display_Help (Cfg);
         raise Stop with "Sys_Interval out of range";
      end if;

      case Verbosity_Level is
      when Integer'First .. -1 | 4 .. Integer'Last =>
         Display_Help (Cfg);
         raise Stop with "verbosity out of range";
      when 0 => Set_Default_Loglevel (Level => Error);
      when 1 => Set_Default_Loglevel (Level => Notice);
      when 2 => Set_Default_Loglevel (Level => Info);
      when 3 => Set_Default_Loglevel (Level => Debug);
      end case;

      L.Log_Message (Debug,
                     "options set to" & Ascii.LF &
                       "port        :" & Port_Int'Img & Ascii.LF &
                       "sys_interval:" & Sys_Interval'Img & Ascii.LF &
                       "verbosity   :" & Verbosity_Level'Img & Ascii.Lf);
   exception
   when Error : GNAT.Command_Line.Exit_From_Command_Line => null;
   when Error : GNAT.Command_Line.Invalid_Switch =>
      Put_Line (Standard_Error, "invalid switch");
      Display_Help (Cfg);
      Set_Exit_Status (Failure);
      raise Stop;
   end Set_Cli_Options;


   --
   --  read config files and command line options
   --
   --  The options are read in increasing priority, i.e. higher
   --  priority overrides options set a lower priority.
   --  4: system install config directory (default: /etc/aq/)
   --  3: user config directory (default: ~/.aq/)
   --  2: local directory (default: .)
   --  1: command line options left to right
   procedure Set_Options
   is
   begin
      Set_Cli_Options;
   end Set_Options;

end Opt_Broker;
