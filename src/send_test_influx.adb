
with Ada.Exceptions;
with Ada.Strings.Bounded;
with Ada.Calendar; -- Real_Time;
with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Ada.Streams;                  use Ada.Streams;

with Generic_FIFO;
with Influx;

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;     use Ada.Text_IO.Text_Streams;
----  with GNAT.Exception_Traces;        use GNAT.Exception_Traces;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Strings_Edit.Streams;         use Strings_Edit.Streams;

with GNAT.Sockets.Connection_State_Machine.HTTP_Server;

with GNAT.Sockets.Connection_State_Machine.HTTP_Client.Signaled;
use  GNAT.Sockets.Connection_State_Machine.HTTP_Client.Signaled;

procedure Send_Test_Influx is

   task type Cyclic_Sender is
   end Cyclic_Sender;


   procedure Dump_Headers (Session : HTTP_Session_Signaled) is
      use GNAT.Sockets.Connection_State_Machine.HTTP_Client;
   begin
      Put_Line ("Headers:");
      for Header in Text_Header'Range loop
         if Get_Response_Header (Session, Header) /= "" then
            Put_Line ("   " &  Image (Header) &  ": "
                        &  Get_Response_Header (Session, Header));
         end if;
      end loop;
   end Dump_Headers;


   task body Cyclic_Sender is
      use Ada.Calendar;
      Period          : constant Duration := 60.0;
      Next            : Time               := Clock + 1.0;

      Factory   : aliased Connections_Factory;
      Server    : aliased Connections_Server (Factory'Access, 0);
      Reference : Handle;
      Message   : aliased String_Stream (1024 * 10);

   begin
      Put_Line ("cyclic_sender HTTP POST started");

      Trace_On (Factory  => Factory,
                Received => GNAT.Sockets.Server.Trace_Decoded,
                Sent     => GNAT.Sockets.Server.Trace_Decoded);

      -- Set (Reference, new HTTP_Session_Signaled (Server'Unchecked_Access, 80, 80, 80));
      Ext_Loop:
      begin
         loop
            Cycle:
            declare
               --  Client : HTTP_Session_Signaled renames
               --    HTTP_Session_Signaled (Ptr (Reference).all);
               Client : HTTP_Session_Signaled (Server'Unchecked_Access, 8000, 8000, 8000);
               use GNAT.Sockets.Connection_State_Machine.HTTP_Server;

            begin
               Put_Line ("waiting ...");
               delay until Next;
               Next := Next + Period;
               Put_Line ("cyclic_sender active");

               Connect (Client, "z3-2", 8086);
               Put_Line ("cyclic_sender HTTP client connected" );

               Set_Request_Header (Client, Accept_Header, "*/*");
               Post (Session => Client,
                     Name    => "/write?db=openhab_db",
                     Content => "test_data value=3.14",
                     Message => Message'Unchecked_Access);

               while Is_Active (Client) loop -- Busy waiting for a response
                  delay 0.01;
               end loop;
               Dump_Headers (Client);
               Put_Line (Image (Get_Response_Code (Client)) &  " "
                           &  Get_Response_Reason (Client)
                           &  " Message >>>>>>>>>>>>>>>>>>>>");
               Put_Line (Get (Message));
               Put_Line ("<<<<<<<<<<<<<<<<<<<< Message");


               --  Build_Message:
               --  declare
               --     use Ada.Calendar.Formatting;
               --     M1 : constant Minute_Number := Minute (Clock);
               --     M2 : constant Minute_Number := Minute (Clock+60.0);
               --     M3 : constant Minute_Number := Minute (Clock+120.0);

               --     T1 : constant String := Image (M1);
               --     T2 : constant String := Image (M2);
               --     T3 : constant String := Image (M3);
               --     Msg1 : constant String := "updatev " & " -t ";
               --     Msg2 : constant String := Msg1 & "T_Vorlauf:T_Heizkoerper:T_Ruecklauf N:";
               --     Msg3 : constant String := Msg2 & T1 & ':' & T2 & ':' & T3 & ASCII.LF;

               --  begin
               --     -- String'Write (Channel, Msg3);
               --     L.Log_Message (Info, "sent '" & Msg3 & "' to " & Image(Address));
               --     -- delay 0.05;

               --     Char_Response:
               --     declare
               --        Data     : Stream_Element_Array (1 .. 1); -- 5350);
               --        Data_Len : Stream_Element_Count;
               --        C_Old    : Character := ' ';
               --        C        : Character;
               --     begin
               --        loop
               --           Ada.Streams.Read (Channel.all, Data, Data_Len);
               --           exit when Data_Len = 0;
               --           C := Character'Val (Data (1));
               --           exit when C_Old = 'O' and C = 'K';
               --           C_Old := C;
               --           Ada.Text_IO.Put (C);
               --        end loop;
               --     end Char_Response;

               --  end Build_Message;

               -- Close_Socket (Cyc_Snd);

            exception
            when E : others =>
               Put_Line ("error in Cycle");
               Put_Line (Ada.Exceptions.Exception_Information (E));
            end Cycle;
         end loop;
      end Ext_Loop;

   end Cyclic_Sender;

   Cyc_Snd : Cyclic_Sender;
begin
   loop
      null;
   end loop;
end Send_Test_Influx;