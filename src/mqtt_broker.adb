--                                                                    --
--  package Test_MQTT_Servers       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2016       --
--                                                                    --
--                                Last revision :  18:59 21 Mar 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Streams;               use Ada.Streams;
with GNAT.Sockets;              use GNAT.Sockets;
with GNAT.Sockets.MQTT.Server;  use GNAT.Sockets.MQTT.Server;
with GNAT.Sockets.Server;       use GNAT.Sockets.Server;

with GNAT.Exception_Traces;

with Strings_Edit.Integers;        use Strings_Edit.Integers;

with Alog;                         use Alog;
with Alog.Logger;
with Alog.Policy_DB;
with Log;                          use Log;

with Ada.Text_Io;

package body MQTT_Broker is

   function Create (Factory  : access Broker_Factory;
                    Listener : access Connections_Server'Class;
                    From     : Sock_Addr_Type)
                   return Connection_Ptr is
   begin
      return
        new Test_Server (Server               => Factory.Server'Unchecked_Access,
                         Listener             => Listener,
                         Input_Size           => 80,
                         Output_Size          => 80,
                         Max_Subscribe_Topics => 20);
   end Create;


   procedure Start_Broker
   is
   begin

      Start_Up_Time := Ada.Calendar.Clock;

      if Policy_DB.Get_Default_Loglevel = Debug then
         Trace_On (Factory  => Factory,
                   Received => GNAT.Sockets.Server.Trace_Decoded,
                   Sent     => GNAT.Sockets.Server.Trace_Decoded);
         Set_Tracing_Flags (Factory.Server, Trace_All);
         GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
      else
         Trace_Off (Factory);
      end if;

   end Start_Broker;


   procedure Publish_Broker_Internals
   is
      use Ada.Text_Io;
   begin
      --  Internals published by the mosquitto broker:
      --
      --  $SYS/broker/bytes/received
      --  The total number of bytes received since the broker started.

      --  $SYS/broker/bytes/sent
      --  The total number of bytes sent since the broker started.

      --  $SYS/broker/clients/connected
      --  The number of currently connected clients.

      --  $SYS/broker/clients/expired
      --  The number of disconnected persistent clients that have been
      --  expired and removed through the persistent_client_expiration
      --  option.

      --  $SYS/broker/clients/disconnected
      --  The total number of persistent clients (with clean session
      --  disabled) that are registered at the broker but are
      --  currently disconnected.

      --  $SYS/broker/clients/maximum
      --  The maximum number of clients that have been connected to
      --  the broker at the same time.

      --  $SYS/broker/clients/total
      --  The total number of active and inactive clients currently
      --  connected and registered on the broker.

      --  $SYS/broker/connection/#
      --  When bridges are configured to/from the broker, common
      --  practice is to provide a status topic that indicates the
      --  state of the connection. This is provided within
      --  $SYS/broker/connection/ by default. If the value of the
      --  topic is 1 the connection is active, if 0 then it is not
      --  active. See the Bridges section below for more information
      --  on bridges.

      --  $SYS/broker/heap/current size
      --  The current size of the heap memory in use by
      --  mosquitto. Note that this topic may be unavailable depending
      --  on compile time options.

      --  $SYS/broker/heap/maximum size
      --  The largest amount of heap memory used by mosquitto. Note
      --  that this topic may be unavailable depending on compile time
      --  options.

      --  $SYS/broker/load/connections/+
      --  The moving average of the number of CONNECT packets received
      --  by the broker over different time intervals. The final "+"
      --  of the hierarchy can be 1min, 5min or 15min. The value
      --  returned represents the number of connections received in 1
      --  minute, averaged over 1, 5 or 15 minutes.

      --  $SYS/broker/load/bytes/received/+
      --  The moving average of the number of bytes received by the
      --  broker over different time intervals. The final "+" of the
      --  hierarchy can be 1min, 5min or 15min. The value returned
      --  represents the number of bytes received in 1 minute,
      --  averaged over 1, 5 or 15 minutes.

      --  $SYS/broker/load/bytes/sent/+
      --  The moving average of the number of bytes sent by the broker
      --  over different time intervals. The final "+" of the
      --  hierarchy can be 1min, 5min or 15min. The value returned
      --  represents the number of bytes sent in 1 minute, averaged
      --  over 1, 5 or 15 minutes.

      --  $SYS/broker/load/messages/received/+
      --  The moving average of the number of all types of MQTT
      --  messages received by the broker over different time
      --  intervals. The final "+" of the hierarchy can be 1min, 5min
      --  or 15min. The value returned represents the number of
      --  messages received in 1 minute, averaged over 1, 5 or 15
      --  minutes.

      --  $SYS/broker/load/messages/sent/+
      --  The moving average of the number of all types of MQTT
      --  messages sent by the broker over different time
      --  intervals. The final "+" of the hierarchy can be 1min, 5min
      --  or 15min. The value returned represents the number of
      --  messages send in 1 minute, averaged over 1, 5 or 15 minutes.

      --  $SYS/broker/load/publish/dropped/+
      --  The moving average of the number of publish messages dropped
      --  by the broker over different time intervals. This shows the
      --  rate at which durable clients that are disconnected are
      --  losing messages. The final "+" of the hierarchy can be 1min,
      --  5min or 15min. The value returned represents the number of
      --  messages dropped in 1 minute, averaged over 1, 5 or 15
      --  minutes.

      --  $SYS/broker/load/publish/received/+
      --  The moving average of the number of publish messages
      --  received by the broker over different time intervals. The
      --  final "+" of the hierarchy can be 1min, 5min or 15min. The
      --  value returned represents the number of publish messages
      --  received in 1 minute, averaged over 1, 5 or 15 minutes.

      --  $SYS/broker/load/publish/sent/+
      --  The moving average of the number of publish messages sent by
      --  the broker over different time intervals. The final "+" of
      --  the hierarchy can be 1min, 5min or 15min. The value returned
      --  represents the number of publish messages sent in 1 minute,
      --  averaged over 1, 5 or 15 minutes.

      --  $SYS/broker/load/sockets/+
      --  The moving average of the number of socket connections
      --  opened to the broker over different time intervals. The
      --  final "+" of the hierarchy can be 1min, 5min or 15min. The
      --  value returned represents the number of socket connections
      --  in 1 minute, averaged over 1, 5 or 15 minutes.

      --  $SYS/broker/messages/inflight
      --  The number of messages with QoS>0 that are awaiting
      --  acknowledgments.

      --  $SYS/broker/messages/received
      --  The total number of messages of any type received since the broker started.

      --  $SYS/broker/messages/sent
      --  The total number of messages of any type sent since the broker started.

      --  $SYS/broker/messages/stored
      --  The number of messages currently held in the message
      --  store. This includes retained messages and messages queued
      --  for durable clients.

      --  $SYS/broker/publish/messages/dropped
      --  The total number of publish messages that have been dropped
      --  due to inflight/queuing limits. See the
      --  max_inflight_messages and max_queued_messages options in
      --  mosquitto.conf(5) for more information.

      --  $SYS/broker/publish/messages/received
      --  The total number of PUBLISH messages received since the
      --  broker started.

      --  $SYS/broker/publish/messages/sent
      --  The total number of PUBLISH messages sent since the broker started.

      --  $SYS/broker/retained messages/count
      --  The total number of retained messages active on the broker.

      --  $SYS/broker/subscriptions/count
      --  The total number of subscriptions active on the broker.

      --  $SYS/broker/timestamp
      --  The timestamp at which this particular build of the broker was made. Static.

      --  $SYS/broker/uptime
      --  The amount of time in seconds the broker has been online.

      --  $SYS/broker/version
      --  The version of the broker. Static.

      -- Put_Line ("(Publish_broker_internals)");
      -- Publish (Factory.Server, "$SYS/broker/clients/connected", "1");
      Publish (Factory.Server, "$SYS/broker/messages/retained",
               Image (Get_Messages_Number (Factory.Server)));

      Uptime:
      declare
         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Uptime : Duration := Now - Start_Up_Time;
      begin
         Publish (Factory.Server, "$SYS/broker/uptime",
                  Image (Integer (Uptime)));
      end Uptime;

      Publish (Factory.Server, "$SYS/broker/version", Aq_Broker_Version);
   end Publish_Broker_Internals;


end MQTT_Broker;
