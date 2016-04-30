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

with Ada.Task_Identification;

with Alog.Exceptions;
with Alog.Facilities;
with Alog.Transforms;

--  Tasked Logger instance. Facilities can be attached to this logger instance
--  in order to log to different targets simultaneously. This instance provides
--  task-safe concurrent logging.
package Alog.Tasked_Logger is

   type Facility_Update_Handle is not null access
     procedure (Facility_Handle : Facilities.Handle);
   --  Handle to facility update procedure.

   task type Instance (Init : Boolean := False) is

      entry Attach_Facility (Facility : Facilities.Handle);
      --  Attach a facility to tasked logger instance.

      entry Attach_Default_Facility;
      --  Attach default facility to tasked logger instance.

      entry Detach_Facility (Name : String);
      --  Detach a facility from tasked logger instance.

      entry Detach_Default_Facility;
      --  Detach default facility from tasked logger instance.

      entry Facility_Count (Count : out Natural);
      --  Return number of attached facilites.

      entry Update
        (Name    : String;
         Process : Facility_Update_Handle);
      --  Update a specific facility identified by 'Name'. Calls the 'Process'
      --  procedure to perform the update operation.

      entry Iterate (Process : Facility_Update_Handle);
      --  Call 'Process' for all attached facilities.

      entry Attach_Transform (Transform : Transforms.Handle);
      --  Attach a transform to tasked logger instance.

      entry Detach_Transform (Name : String);
      --  Detach a transform from tasked logger instance.

      entry Transform_Count (Count : out Natural);
      --  Return number of attached transforms.

      entry Log_Message
        (Level  : Log_Level;
         Msg    : String;
         Source : String := "";
         Caller : Ada.Task_Identification.Task_Id :=
           Ada.Task_Identification.Null_Task_Id);
      --  Log a message. The Write_Message() procedure of all attached
      --  facilities is called. Depending on the Log-Threshold set, the message
      --  is logged to different targets (depending on the facilites)
      --  automatically. If an exception occurs, the exception handler
      --  procedure is called.
      --
      --  If caller is not specified the executing task's ID is used instead.
      --  Since Log_Message'Caller can not be used as default parameter the
      --  entry checks if the variable is set to 'Null_Task_Id' in the body.

      entry Clear;
      --  Clear tasked logger instance. Detach and teardown all attached
      --  facilities and transforms and clear any stored exceptions.

      entry Shutdown;
      --  Explicitly shutdown tasked logger.

      entry Set_Except_Handler (Proc : Exceptions.Exception_Handler);
      --  Set custom exception handler procedure.

   end Instance;
   --  Tasked logger instance. The Init discriminant defines whether or not a
   --  default 'stdout' (FD facility without logfile set) is attached
   --  automatically. Default is 'False'. Set Init to 'True' if you want to
   --  make sure minimal stdout logging is possible as soon as a new logger is
   --  instantiated.
   --
   --  By default exceptions which occur during asynchronous processing are
   --  printed to standard error. Use the Set_Except_Handler entry to register
   --  a custom exception handler.

   type Handle is access all Instance;
   --  Handle to tasked logger type.

end Alog.Tasked_Logger;
