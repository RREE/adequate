--  options for mqtt2influx

with Ada.Command_Line;             use Ada.Command_Line;
with GNAT.Command_Line;            use GNAT.Command_Line;
-- with GNAT.Exception_Traces;
with Ada.Text_IO;                  use Ada.Text_IO;
-- with Ada.Exceptions;               use Ada.Exceptions;
with Alog;                         use Alog;
with Alog.Policy_DB;               use Alog.Policy_DB;
with Logs;                         use Logs;
with Ada.Text_IO;
with Ada.Directories;
with POSIX.User_Database;
with POSIX.Process_Identification;

package body Opt_M2I is

   Cfg : Command_Line_Configuration;
   Stop : exception;

   procedure Set_Options
   is
   begin
      Define_Switch (Cfg, Daemon'Access, "-d",
                     Long_Switch => "--daemon",
                     Help        => "Run mqtt2influx as a daemon.");

      Define_Switch (Cfg, User'Access, "-u:",
                     Long_Switch => "--user=",
                     Argument    => "user login mame",
                     Help        => "The user name that the daemon runs as.");

      Define_Switch (Cfg, Config_File'Access, "-c:",
                        Long_Switch => "--config=",
                        Argument    => "filename",
                        Help        => "Filename of the configuration file.");

      Define_Switch (Cfg, Verbosity_Level'Access, "-v?",
                     Long_Switch => "--verbose?",
                     Initial     => 1,
                     Default     => 2,
                     Argument    => "INT",
                     Help        => "Verbosity level: 0: quiet, 1: normal, 2: extended, 3: debug");

      Define_Switch (Cfg, "-h", Long_Switch => "--help", Help => "show this text");

      Set_Usage (Cfg,
                 Usage => "[switches]",
                 Help  => "subscribe to MQTT topics and send the values to a InfluxDB.");

      Getopt (Cfg);


      --
      -- check valid option ranges and convert to the target type
      --
      case Verbosity_Level is
      when Integer'First .. -1 | 4 .. Integer'Last =>
         Display_Help (Cfg);
         raise Stop with "verbosity out of range";
      when 0 => Set_Default_Loglevel (Level => Error);
      when 1 => Set_Default_Loglevel (Level => Notice);
      when 2 => Set_Default_Loglevel (Level => Info);
      when 3 => Set_Default_Loglevel (Level => Debug);
      end case;

      if Config_File = null or else Config_File.all = "" then
         if Daemon then
            Config_File := new String'("/etc/mqtt2influx.cfg");
         else
            Config_File := new String'("./mqtt2influx.cfg");
         end if;
      end if;
      Check_Config_File:
      declare
         use Ada.Directories;
         use Ada.Text_IO;
      begin
         if not Exists (Config_File.all) then
            Put_Line (Standard_Error, "the file '" & Config_File.all & "' does not exist!");
            raise Stop;
         end if;
      end Check_Config_File;

      if Daemon and then (User = null or else User.all = "") then
         declare
            use Ada.Text_IO;
         begin
            Put_Line (Standard_Error, "you must provide a user when in daemon mode.");
         end;
         raise Stop;
      end if;

      --  Check_User:
      --  declare
      --     use Ada.Text_IO;
      --     use POSIX;
      --     use POSIX.User_Database;
      --     use POSIX.Process_Identification;
      --     Item : constant User_Database_Item := Get_User_Database_Item (To_POSIX_String(User.all));
      --     UID : constant User_ID := User_ID_Of (Item);
      --  begin
      --     null;
      --  exception
      --  when POSIX_Error =>
      --     Put_Line (Standard_Error, "user '" & User.all & "' does not exist.");
      --     raise Stop;
      --  end Check_User;
--      L.Log_Message (Debug,
Ada.Text_IO.Put_Line(
                     "options set to" & ASCII.LF &
                       "user     : " & User.all & ASCII.LF &
                       "daemon   : " & Daemon'Img & ASCII.LF &
                       "config   : " & Config_File.all & ASCII.LF &
                       "verbosity: " & Verbosity_Level'Img & ASCII.LF);
   exception
   when Error : GNAT.Command_Line.Exit_From_Command_Line => null;
   when Error : GNAT.Command_Line.Invalid_Switch =>
      L.Log_Message (Alog.Error, "invalid switch");
      Ada.Text_IO.Put_Line (Standard_Error, "invalid switch");
      Display_Help (Cfg);
      Set_Exit_Status (Failure);
      raise Stop;
   end Set_Options;

end Opt_M2I;
