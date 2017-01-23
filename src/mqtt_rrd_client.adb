--                                                                    --
--  package MQTT_Clients            Copyright (c)  Dmitry A. Kazakov  --
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

-- 2016-12: R. Ebert, use alog instead of Text_IO

with Ada.Exceptions;
with Ada.Strings.Bounded;
with Ada.Real_Time;
with Alog;                         use Alog;
with Alog.Logger;                  use Alog.Logger;
with Logs;                         use Logs;
with GNAT.Sockets;                 use GNAT.Sockets;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Generic_FIFO;
with Rrd;

package body MQTT_Rrd_Client is

   Max_Message_Length : constant := 2000;
   Max_Topic_Length   : constant := 1000;
   Max_Config_Length  : constant := 100;

   package Topic_Strings   is new Ada.Strings.Bounded.Generic_Bounded_Length (Max_Topic_Length);
   subtype Topic_Str is Topic_Strings.Bounded_String;
   function "+" (Source : Topic_Str) return String
     renames Topic_Strings.To_String;
   function "+" (Source : String) return Topic_Str;

   package Message_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max_Message_Length);
   subtype Message_Str is Message_Strings.Bounded_String;
   function "+" (Source : Message_Str) return String
     renames Message_Strings.To_String;
   function "+" (Source : Stream_Element_Array) return Message_Str;

   package Config_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max_Config_Length);
   subtype Config_Str is Config_Strings.Bounded_String;
   function "+" (Source : Config_Str) return String
     renames Config_Strings.To_String;
   function "+" (Source : String) return Config_Str;


   type Topic_And_Message is record
      Topic   : Topic_Str;
      Message : Message_Str;
   end record;


   package Msg_FIFO is new Generic_FIFO (Topic_And_Message);

   Msg_Q : Msg_FIFO.FIFO (Size => 100);


   ------------------------------------------------------------------------------
   function "+" (Source : String) return Topic_Strings.Bounded_String
   is begin
      return Topic_Strings.To_Bounded_String (Source);
   end "+";

   function "+" (Source : Stream_Element_Array) return Message_Strings.Bounded_String
   is begin
      return Message_Strings.To_Bounded_String (Image (Source));
   end "+";

   function "+" (Source : String) return Config_Strings.Bounded_String
   is begin
      return Config_Strings.To_Bounded_String (Source);
   end "+";

   ------------------------------------------------------------------------------


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
      L.Log_Message (Warning, "Connect rejected " & Image (Response));
   end On_Connect_Rejected;


   --  procedure On_Publish (Pier      : in out MQTT_Client;
   --                        Topic     : String;
   --                        Message   : Stream_Element_Array;
   --                        Packet    : Packet_Identification;
   --                        Duplicate : Boolean;
   --                        Retain    : Boolean)
   --  is
   --     Address  : Sock_Addr_Type;
   --     Socket   : Socket_Type;
   --     Channel  : Stream_Access;
   --  begin
   --     L.Log_Message (Info, "received " & Topic & " = " & Image (Message));
   --     On_Publish (MQTT_Pier (Pier),
   --                 Topic,
   --                 Message,
   --                 Packet,
   --                 Duplicate,
   --                 Retain);

   --     declare
   --        Rrdserv : constant String  := Rrd.Rrdserv (Topic);
   --        Rrdport : constant Natural := Rrd.Rrdport (Topic);
   --        Rrdfile : constant String  := Rrd.Rrdfile (Topic);
   --        Rrdname : constant String  := Rrd.Rrdname (Topic);

   --        Value_Str : constant String := Image (Message);

   --        Message : constant String := "update " & Rrdfile & " -t " & Rrdname
   --          & " N:" & Value_Str;
   --     begin
   --        Address.Addr := Addresses (Get_Host_By_Name (Rrdserv));
   --        Address.Port := Port_Type (Rrdport);
   --        Create_Socket (Socket);
   --        -- Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
   --        Connect_Socket (Socket, Address);
   --        Channel := Stream(Socket);
   --        String'Write (Channel, Message);
   --        Close_Socket (Socket);

   --        L.Log_Message (Info, "sending '" & Message & "' to " & Rrdserv & ':' & Image (Rrdport));
   --     end;

   --  exception
   --  when E : others =>
   --     L.Log_Message (Error, Ada.Exceptions.Exception_Information (E));
   --     raise;
   --  end On_Publish;


   procedure On_Publish (Pier      : in out MQTT_Client;
                         Topic     : String;
                         Message   : Stream_Element_Array;
                         Packet    : Packet_Identification;
                         Duplicate : Boolean;
                         Retain    : Boolean)
   is
   begin
      L.Log_Message (Info, "received " & Topic & " = " & Image (Message));
      On_Publish (MQTT_Pier (Pier),
                  Topic,
                  Message,
                  Packet,
                  Duplicate,
                  Retain);

      Msg_FIFO.Put (Msg_Q, (+Topic, +Message));
      L.Log_Message (Info, "added ("&Topic&"), ("&(+(+Message))&") to Msg_Q");
      L.Log_Message (Info, "Msg_Q has "&Image(Msg_Q.Length)&" elements");

   exception
   when E : others =>
      L.Log_Message (Error, Ada.Exceptions.Exception_Information (E));
      raise;
   end On_Publish;


   ------------------------------------------------------------------------------

   task body Cyclic_Sender is
      use Ada.Real_Time;
      Period          : constant Time_Span := Seconds (60);
      Next            : Time               := Clock + Period;
      Nr_Of_Msgs      : Natural;
      Address         : Sock_Addr_Type;
      Socket          : Socket_Type;
      Channel         : Stream_Access;
      -- Rrdserv : constant String  := Rrd.Rrdserv (Topic);
      -- Rrdport : constant Natural := Rrd.Rrdport (Topic);
      Rrdfile         : Config_Strings.Bounded_String;
      Server_Port_Str : Config_Strings.Bounded_String;

      Names, Values   : Message_Strings.Bounded_String;

   begin
      L.Log_Message (Info, "cyclic_sender started");
      accept Set_Config  (Server_Name : String;
                          Server_Port : Port_Type;
                          Filename    : String)
      do
         Address.Addr := Addresses (Get_Host_By_Name (Server_Name));
         Address.Port := Server_Port;
         Rrdfile      := Config_Strings.To_Bounded_String(Filename);
         Server_Port_Str := +(Server_Name & ':' & Image(Integer(Server_Port)));
      end Set_Config;


      loop
         delay until Next;
         Next := Next + Period;
         L.Log_Message (Info, "cyclic_sender active");

         Nr_Of_Msgs := Msg_Q.Length;

         Names := Message_Strings.Null_Bounded_String;
         Values := Message_Strings.Null_Bounded_String;
         for M in 1 .. Nr_Of_Msgs loop
            declare
               M : constant Topic_And_Message := Msg_Q.Get;

               Rrdname   : constant String := Rrd.Rrdname (+M.Topic);
               Value_Str : constant String := +M.Message;

            begin
               Message_Strings.Append (Names, ':');
               Message_Strings.Append (Names, Rrdname);
               Message_Strings.Append (Values, ':');
               Message_Strings.Append (Values, Value_Str);
            end;
         end loop;

         Build_Rrd_Message:
         declare
            use Message_Strings;
            Message : Message_Str := Null_Bounded_String;
         begin
            Append (Message, "update " & (+Rrdfile) & " -t ");
            --  remove leading colon from the names
            Append (Message, Slice(Names, 2, Length(Names)));
            Append (Message, " N");
            Append (Message, +Values);
            Append (Message, ASCII.LF);

            Create_Socket (Socket);
            -- Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
            Connect_Socket (Socket, Address);
            Channel := Stream(Socket);
            L.Log_Message (Info, "cyclic_sender created socket to " & (+Server_Port_Str));
            String'Write (Channel, +Message);
            Close_Socket (Socket);

            L.Log_Message (Info, "sending '" & (+Message) & "' to " & (+Server_Port_Str));
         end Build_Rrd_Message;
      end loop;
   end Cyclic_Sender;

end MQTT_Rrd_Client;
