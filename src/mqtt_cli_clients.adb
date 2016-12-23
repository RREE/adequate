--                                                                    --
--  package MQTT_Cli_Clients        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

with Alog;                         use Alog;
with Alog.Logger;                  use Alog.Logger;
with Logs;                         use Logs;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with Strings_Edit.Integers;        use Strings_Edit.Integers;

package body MQTT_Cli_Clients is

   procedure On_Connect_Accepted (Pier            : in out MQTT_Client;
                                  Session_Present : Boolean)
   is
   begin
      L.Log_Message (Info, "MQTT connect accepted");
   end On_Connect_Accepted;


   procedure On_Connect_Rejected (Pier     : in out MQTT_Client;
                                  Response : Connect_Response)
   is
   begin
      L.Log_Message (Info, "Connect rejected " & Image (Response));
   end On_Connect_Rejected;


   procedure On_Ping_Response (Pier : in out MQTT_Client) is
   begin
      L.Log_Message (Info, "Ping response");
   end On_Ping_Response;


   procedure On_Publish (Pier      : in out MQTT_Client;
                         Topic     : String;
                         Message   : Stream_Element_Array;
                         Packet    : Packet_Identification;
                         Duplicate : Boolean;
                         Retain    : Boolean)
   is
   begin
      L.Log_Message (Info, "Message " & Topic & " = " & Image (Message));
      On_Publish (MQTT_Pier (Pier),
                  Topic,
                  Message,
                  Packet,
                  Duplicate,
                  Retain);
   end On_Publish;


   procedure On_Subscribe_Acknowledgement (Pier   : in out MQTT_Client;
                                           Packet : Packet_Identifier;
                                           Codes  : Return_Code_List)
   is

   begin
      -- for Index in Codes'Range loop
      for Code of Codes loop
         if Code.Success then
            L.Log_Message (Info, "Subscribed " & Image (Integer (Packet))
                             & ":" & QoS_Level'Image (Code.QoS));
         else
            L.Log_Message (Warning, "Subscribe " & Image (Integer (Packet))
                             & ": failed");
         end if;
         --  for Index in Codes'Range loop
         --     if Index /= Codes'First then
         --        Put (", ");
         --     end if;
         --     if Codes (Index).Success then
         --        Put (QoS_Level'Image (Codes (Index).QoS));
         --     else
         --        Put ("Failed");
         --     end if;
      end loop;
   end On_Subscribe_Acknowledgement;

end MQTT_Cli_Clients;
