--  options for aq_pub

with Ada.Command_Line;             use Ada.Command_Line;
with GNAT.Command_Line;            use GNAT.Command_Line;
with GNAT.Exception_Traces;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Exceptions;               use Ada.Exceptions;
with Alog;                         use Alog;
with Alog.Policy_DB;               use Alog.Policy_DB;
with Logs;                         use Logs;

package body Opt_Cli_Client is

   Cfg : Command_Line_Configuration;
   Stop : exception;

   procedure Set_Options
   is
   begin
      -- GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
      Define_Switch (Cfg, Port_Int'Access, "-p:",
                     Long_Switch => "--port=",
                     Initial     => 1883,
                     Default     => 1883,
                     Argument    => "INT",
                     Help        => "MQTT server port (default: 1883).");

      Define_Switch (Cfg, Server_Name'Access, "-H:",
                     Long_Switch => "--host=",
                     Argument    => "FQN|IP",
                     Help        => "The MQTT server name or IP address (default: localhost).");

      if Prg_Type = Publisher then
         Define_Switch (Cfg, Topic_Text'Access, "-t:",
                        Long_Switch => "--topic=",
                        Argument    => """TEXT""",
                        Help        => "The MQTT topic on which to publish the message.");
      else
         Define_Switch (Cfg, Topic_Text'Access, "-t:",
                        Long_Switch => "--topic=",
                        Argument    => """TEXT""",
                        Help        => "The MQTT topic of topic pattern for subscription.");
      end if;

      if Prg_Type = Publisher then
         Define_Switch (Cfg, Message_Text'Access, "-m:",
                        Long_Switch => "--message=",
                        Argument    => """TEXT""",
                        Help        => "A single message to send.");
      end if;

      Define_Switch (Cfg, Message_Text'Access, "-c:",
                     Long_Switch => "--client=",
                     Argument    => """TEXT""",
                     Help        => "The client id when connecting to the server (default: aq_(p|s)ub)");

      if Prg_Type = Subscriber then
         Define_Switch (Cfg, Port_Int'Access, "-C:",
                        Long_Switch => "--count=",
                        Initial     => 1,
                        Default     => 1,
                        Argument    => "INT",
                        Help        => "stop after count message have been received (default: 1).");
      end if;

      Define_Switch (Cfg, QoS_Int'Access, "-q:",
                     Long_Switch => "--QoS=",
                     Initial     => 0,
                     Default     => 0,
                     Argument    => "INT",
                     Help        => "Quality of Service: 0: at most once, " &
                       "1: at least once, 2: exactly once");

      Define_Switch (Cfg, Verbosity_Level'Access, "-v?",
                     Long_Switch => "--verbose?",
                     Initial     => 1,
                     Default     => 2,
                     Argument    => "INT",
                     Help        => "Verbosity level: 0: quiet, 1: normal, 2: extended, 3: debug");

      Define_Switch (Cfg, "-h", Long_Switch => "--help", Help => "show this text");
      if Prg_Type = Publisher then
         Set_Usage (Cfg,
                    Usage => "[switches], topic and message must not be empty.",
                    Help  => "simple MQTT client to publish messages.");
      else
         Set_Usage (Cfg,
                    Usage => "[switches], topic must not be empty.",
                    Help  => "simple MQTT subscribe client.");
      end if;

      Getopt (Cfg);


      --
      -- check valid option ranges and convert to the target type
      --
      Port := GNAT.Sockets.Port_Type (Port_Int);

      if Topic_Text = null or else Topic_Text.all = "" then
         Display_Help (Cfg);
         raise Stop with "empty topic";
      end if;
      if Prg_Type = Publisher and then (Message_Text = null or else Message_Text.all = "") then
         Display_Help (Cfg);
         raise Stop with "empty message";
      end if;

      case QoS_Int is
      when Integer'First .. -1 | 3 .. Integer'Last =>
         Display_Help (Cfg);
         raise Stop with "QoS out of range";
      when 0 => QoS := At_Most_Once;
      when 1 => QoS := At_Least_Once;
      when 2 => QoS := Exactly_Once;
      end case;

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
                     "options set to" & ASCII.LF &
                       "port     :" & Port_Int'Img & ASCII.LF &
                       "server   : " & Server_Name.all & ASCII.LF &
                       "topic    : " & Topic_Text.all & ASCII.LF &
                       (if Prg_Type = Publisher then
                          "message  : " & Message_Text.all & ASCII.LF
                        else "") &
                       "QoS      : " & Image(QoS) & ASCII.LF &
                       "client   : " & Client_Name.all & ASCII.LF &
                       "verbosity:" & Verbosity_Level'Img & ASCII.LF);
   exception
   when Error : GNAT.Command_Line.Exit_From_Command_Line => null;
   when Error : GNAT.Command_Line.Invalid_Switch =>
      L.Log_Message (Alog.Error, "invalid switch");
      Ada.Text_IO.Put_Line (Standard_Error, "invalid switch");
      Display_Help (Cfg);
      Set_Exit_Status (Failure);
      raise Stop;
   end Set_Options;

end Opt_Cli_Client;
