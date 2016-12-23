--
--  Copyright (c) 2009-2012,
--  Reto Buerki, Adrian-Ken Rueegsegger
--
--  This file is part of Alog.
--
--  Alog is free software; you can redistribute it and/or modify
--  it under the terms of the GNU Lesser General Public License as published
--  by the Free Software Foundation; either version 2.1 of the License, or
--  (at your option) any later version.
--
--  Alog is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Lesser General Public License for more details.
--
--  You should have received a copy of the GNU Lesser General Public License
--  along with Alog; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
--  MA  02110-1301  USA
--

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Alog.Logger;

package body Alog.Tasked_Logger is

   use Ada.Strings.Unbounded;

   procedure F_Dummy (Facility_Handle : Facilities.Handle) is null;
   --  This procedure is needed to initialize the 'Current_Facility_Proc'
   --  handle of type Facility_Update_Handle since that type is defined as
   --  'not null'.

   procedure Default_Handler
     (Except : Ada.Exceptions.Exception_Occurrence;
      Caller : Ada.Task_Identification.Task_Id);
   --  Tasked logger default exception handling callback. Prints the calling
   --  task's ID and exception information to stderr.

   -------------------------------------------------------------------------

   procedure Default_Handler
     (Except : Ada.Exceptions.Exception_Occurrence;
      Caller : Ada.Task_Identification.Task_Id)
   is
      use Ada.Task_Identification;
   begin
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Current_Error,
         Item => "Logging exception while processing request for task with ID "
         & Image (T => Caller));
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Current_Error,
         Item => Ada.Exceptions.Exception_Information (Except));
   end Default_Handler;

   -------------------------------------------------------------------------

   task body Instance is
      use type Ada.Task_Identification.Task_Id;

      Except_Handler : Exceptions.Exception_Handler := Default_Handler'Access;
      --  Exception handler callback, initialized to default handler.

      Logsink               : Alog.Logger.Instance (Init => Init);
      Current_Source        : Unbounded_String;
      Current_Level         : Log_Level;
      Current_Message       : Unbounded_String;
      Current_Caller        : Ada.Task_Identification.Task_Id;
      Current_Facility_Name : Unbounded_String;
      Current_Facility_Proc : Facility_Update_Handle := F_Dummy'Access;
   begin

      Main_Loop :
      loop
         begin
            select

               -------------------------------------------------------------

               accept Attach_Facility (Facility : Facilities.Handle) do
                  Logsink.Attach_Facility (Facility => Facility);
               end Attach_Facility;
            or

               -------------------------------------------------------------

               accept Attach_Default_Facility do
                  Logsink.Attach_Default_Facility;
               end Attach_Default_Facility;
            or

               -------------------------------------------------------------

               accept Detach_Facility (Name : String) do
                  Logsink.Detach_Facility (Name => Name);
               end Detach_Facility;
            or

               -------------------------------------------------------------

               accept Detach_Default_Facility do
                  Logsink.Detach_Default_Facility;
               end Detach_Default_Facility;
            or

               -------------------------------------------------------------

               accept Facility_Count (Count : out Natural) do
                  Count := Logsink.Facility_Count;
               end Facility_Count;
            or

               -------------------------------------------------------------

               accept Update
                 (Name    : String;
                  Process : Facility_Update_Handle)
               do
                  Current_Facility_Name := To_Unbounded_String (Name);
                  Current_Facility_Proc := Process;
                  Current_Caller        := Instance.Update'Caller;
               end Update;

               begin
                  Logsink.Update (Name    => To_String (Current_Facility_Name),
                                  Process => Current_Facility_Proc);

               exception
                  when E : others =>
                     Except_Handler (Except => E,
                                     Caller => Current_Caller);
               end;
            or

               -------------------------------------------------------------

               accept Iterate (Process : Facility_Update_Handle) do
                  Current_Facility_Proc := Process;
                  Current_Caller        := Instance.Iterate'Caller;
               end Iterate;

               begin
                  Logsink.Iterate (Process => Current_Facility_Proc);

               exception
                  when E : others =>
                     Except_Handler (Except => E,
                                     Caller => Current_Caller);
               end;
            or

               -------------------------------------------------------------

               accept Attach_Transform (Transform : Transforms.Handle) do
                  Logsink.Attach_Transform (Transform => Transform);
               end Attach_Transform;
            or

               -------------------------------------------------------------

               accept Detach_Transform (Name : String) do
                  Logsink.Detach_Transform (Name => Name);
               end Detach_Transform;

            or
               -------------------------------------------------------------

               accept Transform_Count (Count : out Natural) do
                  Count := Logsink.Transform_Count;
               end Transform_Count;

            or

               -------------------------------------------------------------

               accept Clear do
                  Logsink.Clear;
               end Clear;
            or

               -------------------------------------------------------------

               accept Log_Message
                 (Level  : Log_Level;
                  Msg    : String;
                  Source : String := "";
                  Caller : Ada.Task_Identification.Task_Id :=
                    Ada.Task_Identification.Null_Task_Id)
               do
                  Current_Source  := To_Unbounded_String (Source);
                  Current_Level   := Level;
                  Current_Message := To_Unbounded_String (Msg);

                  --  Log_Message'Caller can not be used as default parameter
                  --  so we need to check for 'Null_Task_Id' instead.

                  if Caller = Ada.Task_Identification.Null_Task_Id then
                     Current_Caller := Log_Message'Caller;
                  else
                     Current_Caller := Caller;
                  end if;
               end Log_Message;

               begin
                  Logsink.Log_Message
                    (Source => To_String (Current_Source),
                     Level  => Current_Level,
                     Msg    => To_String (Current_Message));

               exception
                  when E : others =>
                     Except_Handler (Except => E,
                                     Caller => Current_Caller);
               end;

            or

               -------------------------------------------------------------

               accept Shutdown;
               exit Main_Loop;

            or

               -----------------------------------------------------------

               accept Set_Except_Handler
                 (Proc : Exceptions.Exception_Handler)
               do
                  Except_Handler := Proc;
               end Set_Except_Handler;

            or
               terminate;
            end select;

            --  Exceptions raised during a rendezvous are raised here and in
            --  the calling task. Catch and ignore it so the tasked logger does
            --  not get terminated after an exception.

         exception
            when others =>
               null;
         end;
      end loop Main_Loop;

   end Instance;

end Alog.Tasked_Logger;
