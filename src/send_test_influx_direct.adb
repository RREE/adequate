
with Ada.Exceptions;
with Ada.Strings.Bounded;
with Ada.Calendar; -- Real_Time;
with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Ada.Streams;                  use Ada.Streams;

--with Generic_FIFO;
-- with Influx;

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;     use Ada.Text_IO.Text_Streams;
with Ada.Streams;
with GNAT.Sockets;                 use GNAT.Sockets;
----  with GNAT.Exception_Traces;        use GNAT.Exception_Traces;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Strings_Edit.Streams;         use Strings_Edit.Streams;


procedure Send_Test_Influx_Direct is

   task type Cyclic_Sender is
   end Cyclic_Sender;


   task body Cyclic_Sender is
      use Ada.Calendar;
      Period          : constant Duration := 2.0;
      Next            : Time               := Clock + 1.0;


      CRLF    : constant String := (1 => ASCII.CR, 2 => ASCII.LF);

      Offset : Ada.Streams.Stream_Element_Count;
      Data   : Ada.Streams.Stream_Element_Array (1 .. 1024);

      -- Response   : aliased String_Stream (1024 * 10);
      Count : Natural := 0;

   begin
      Put_Line ("cyclic_sender HTTP POST started");


      Ext_Loop:
      begin
         loop
            Cycle:
            declare
               --  Client : HTTP_Session_Signaled renames
               --    HTTP_Session_Signaled (Ptr (Reference).all);
               Client  : Socket_Type;
               Address : Sock_Addr_Type;
               Channel : GNAT.Sockets.Stream_Access;
            begin
               GNAT.Sockets.Initialize;  -- initialize a specific package
               Create_Socket (Client);
               Set_Socket_Option (Client, Socket_Level, (Reuse_Address, True));
               Address.Addr := Inet_Addr("192.168.178.10");
               Address.Port := 8086;

               Put_Line ("waiting ...");
               delay until Next;
               Next := Next + Period;
               Put_Line ("cyclic_sender active");

               Connect_Socket (Client, Address);
               Put_Line ("connected");
               Channel := Stream (Client);

               Count := Count + 1;

               String'Write (Channel, "POST /write?db=openhab_db HTTP/1.1" & CRLF &
                               "Host: z3-3:8086" & CRLF &
                                        "Accept: */*" & CRLF &
                                        -- "User-Agent: test_prg/V0.0" & CRLF &
                                        "Content-Type: application/x-www-form-urlencoded" & CRLF &
                                        "Content-Length: 21" & CRLF &
                                        CRLF &
                                        "test_data value=5.56" & Count'Img(2..2));


               --  Put_Line ("sent, waiting for response");

               --  loop
               --     Ada.Streams.Read (Channel.all, Data, Offset);
               --     exit when Offset = 0;
               --     for I in 1 .. Offset loop
               --        Ada.Text_IO.Put (Character'Val (Data (I)));
               --     end loop;
               --  end loop;
               --  Put_Line ("finished");
               Close_Socket (Client);
               Put_Line ("closed");

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
end Send_Test_Influx_Direct;
