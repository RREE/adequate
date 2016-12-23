--
--  Copyright (c) 2009-2015,
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

with Ada.Task_Identification;

with Alog.Log_Request;

package body Alog.Active_Logger is

   -------------------------------------------------------------------------

   procedure All_Done (Logger : in out Instance) is
   begin
      Logger.Message_Queue.All_Done;
   end All_Done;

   -------------------------------------------------------------------------

   procedure Attach_Default_Facility (Logger : in out Instance)
   is
   begin
      Logger.Backend.Attach_Default_Facility;
   end Attach_Default_Facility;

   -------------------------------------------------------------------------

   procedure Attach_Facility
     (Logger   : in out Instance;
      Facility :        Facilities.Handle)
   is
   begin
      Logger.Backend.Attach_Facility (Facility);
   end Attach_Facility;

   -------------------------------------------------------------------------

   procedure Attach_Transform
     (Logger    : in out Instance;
      Transform :        Transforms.Handle)
   is
   begin
      Logger.Backend.Attach_Transform (Transform);
   end Attach_Transform;

   -------------------------------------------------------------------------

   procedure Clear (Logger : in out Instance) is
   begin
      Logger.Backend.Clear;
   end Clear;

   -------------------------------------------------------------------------

   procedure Detach_Default_Facility (Logger : in out Instance)
   is
   begin
      Logger.Backend.Detach_Default_Facility;
   end Detach_Default_Facility;

   -------------------------------------------------------------------------

   procedure Detach_Facility
     (Logger : in out Instance;
      Name   :        String)
   is
   begin
      Logger.Backend.Detach_Facility (Name);
   end Detach_Facility;

   -------------------------------------------------------------------------

   procedure Detach_Transform
     (Logger : in out Instance;
      Name   :        String)
   is
   begin
      Logger.Backend.Detach_Transform (Name);
   end Detach_Transform;

   -------------------------------------------------------------------------

   function Facility_Count (Logger : Instance) return Natural is
      F_Count : Natural;
   begin
      Logger.Backend.Facility_Count (Count => F_Count);
      return F_Count;
   end Facility_Count;

   -------------------------------------------------------------------------

   procedure Finalize (Helper : in out Shutdown_Helper) is
   begin
      Helper.Logger.Shutdown;
   end Finalize;

   -------------------------------------------------------------------------

   function Get_Queue_Length (Logger : Instance) return Natural is
   begin
      return Logger.Message_Queue.Length;
   end Get_Queue_Length;

   -------------------------------------------------------------------------

   function Is_Terminated (Logger : Instance) return Boolean is
   begin
      return Logger.Terminated.State;
   end Is_Terminated;

   -------------------------------------------------------------------------

   procedure Iterate
     (Logger  : in out Instance;
      Process :        Tasked_Logger.Facility_Update_Handle)
   is
   begin
      Logger.Backend.Iterate (Process);
   end Iterate;

   -------------------------------------------------------------------------

   procedure Log_Message
     (Logger : in out Instance;
      Level  :        Log_Level;
      Msg    :        String;
      Source :        String := "")
   is
   begin
      Logger.Message_Queue.Put
        (Element => Log_Request.Create
           (ID      => Ada.Task_Identification.Current_Task,
            Source  => Source,
            Level   => Level,
            Message => Msg));
   end Log_Message;

   -------------------------------------------------------------------------

   procedure Set_Except_Handler
     (Logger : Instance;
      Proc   : Exceptions.Exception_Handler)
   is
   begin
      Logger.Backend.Set_Except_Handler (Proc => Proc);
   end Set_Except_Handler;

   -------------------------------------------------------------------------

   procedure Shutdown
     (Logger : in out Instance;
      Flush  :        Boolean := True)
   is
      Is_Terminated : Boolean;
   begin
      Logger.Terminated.Swap
        (New_State => True,
         Old_State => Is_Terminated);

      if Is_Terminated then
         return;
      end if;

      if Flush then
         Logger.Message_Queue.All_Done;
      else
         Logger.Message_Queue.Clear;
      end if;

      Logger.Message_Queue.Put (Element => Log_Request.Termination_Request);
      Logger.Clear;
      if Logger.Backend'Callable then
         Logger.Backend.Shutdown;
      end if;
   end Shutdown;

   -------------------------------------------------------------------------

   function Transform_Count (Logger : Instance) return Natural is
      T_Count : Natural;
   begin
      Logger.Backend.Transform_Count (Count => T_Count);
      return T_Count;
   end Transform_Count;

   -------------------------------------------------------------------------

   procedure Update
     (Logger  : in out Instance;
      Name    :        String;
      Process :        Tasked_Logger.Facility_Update_Handle)
   is
   begin
      Logger.Backend.Update (Name    => Name,
                             Process => Process);
   end Update;

   -------------------------------------------------------------------------

   protected body Protected_Boolean
   is

      ----------------------------------------------------------------------

      function State return Boolean
      is
      begin
         return S;
      end State;

      ----------------------------------------------------------------------

      procedure Swap
        (New_State :     Boolean;
         Old_State : out Boolean)
      is
      begin
         Old_State := S;
         S         := New_State;
      end Swap;

   end Protected_Boolean;

   -------------------------------------------------------------------------

   task body Logging_Task is
      use type Log_Request.Instance;

      Current_Request : Log_Request.Instance;
   begin
      Log_Loop :
      loop
         begin
            Parent.Message_Queue.Get
              (Element => Current_Request);

            if Current_Request = Log_Request.Termination_Request then
               Parent.Message_Queue.Done;
               exit Log_Loop;
            end if;

            Parent.Backend.Log_Message
              (Source => Current_Request.Get_Source,
               Level  => Current_Request.Get_Log_Level,
               Msg    => Current_Request.Get_Message,
               Caller => Current_Request.Get_Caller_ID);

            Parent.Message_Queue.Done;

         exception
            when Program_Error =>

               --  The Queue has terminated, let's shutdown.

               exit Log_Loop;

            when others =>

               --  Ignore other errors and resume normal operation.

               null;
         end;
      end loop Log_Loop;
   end Logging_Task;

end Alog.Active_Logger;
