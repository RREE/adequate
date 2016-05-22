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
with Ada.Calendar;              use Ada.Calendar;
with GNAT.Sockets;              use GNAT.Sockets;
with GNAT.Sockets.MQTT;         use GNAT.Sockets.MQTT;
with GNAT.Sockets.MQTT.Server;  use GNAT.Sockets.MQTT.Server;
with GNAT.Sockets.Server;       use GNAT.Sockets.Server;

package MQTT_Broker is

   Aq_Broker_Version : constant String := "0.0.1";


   type Broker_Factory is new Connections_Factory with record
      Server : aliased MQTT_Server;
   end record;
   function Create (Factory  : access Broker_Factory;
                    Listener : access Connections_Server'Class;
                    From     : Sock_Addr_Type)
                   return Connection_Ptr;

   type Test_Server is new MQTT_Connection with private;

   procedure Start_Broker;

   procedure Publish_Broker_Internals;

   Factory : aliased Broker_Factory;
   Server  : Connections_Server (Factory'Access, MQTT_Port);

   Start_Up_Time : Ada.Calendar.Time;

private
   type Test_Server is new MQTT_Connection with null record;

end MQTT_Broker;
