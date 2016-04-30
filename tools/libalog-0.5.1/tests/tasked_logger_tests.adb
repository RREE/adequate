--
--  Copyright (c) 2008-2011,
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

with Ada.Exceptions;
with Ada.Task_Identification;

with Ahven;

with Alog.Logger;
with Alog.Tasked_Logger;
with Alog.Facilities.File_Descriptor;
with Alog.Facilities.Syslog;
with Alog.Facilities.Mock;
with Alog.Transforms.Casing;

package body Tasked_Logger_Tests is

   use Ahven;
   use Alog;

   Counter : Natural := 0;
   pragma Atomic (Counter);

   procedure Do_Nothing (Facility_Handle : Facilities.Handle) is null;
   --  Just do nothing.

   procedure Inc_Counter (F_Handle : Facilities.Handle);
   --  Increment iterate counter.

   procedure Raise_Exception (F_Handle : Facilities.Handle);
   --  Raise constraint error.

   procedure Enable_Facility_Timestamp (Facility_Handle : Facilities.Handle);
   --  Enable write timestamp of facility.

   procedure Except_Handler
     (Except : Ada.Exceptions.Exception_Occurrence;
      Caller : Ada.Task_Identification.Task_Id);
   --  Exception handler used in tests. Just increments the atomic counter.

   -------------------------------------------------------------------------

   procedure Attach_Facility is
      Log      : Tasked_Logger.Instance (Init => False);
      Facility : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Count    : Natural := Natural'Last;
   begin
      Log.Facility_Count (Count => Count);

      Assert (Condition => Count = 0,
              Message   => "facility count not 0");

      Log.Attach_Facility (Facility => Facility);
      Log.Facility_Count (Count => Count);
      Assert (Condition => Count = 1,
              Message => "could not attach facility");

      begin
         Log.Attach_Facility (Facility => Facility);
         Fail (Message => "attached duplicate facility");

      exception
         when Logger.Facility_Already_Present =>
            null;
      end;
   end Attach_Facility;

   -------------------------------------------------------------------------

   procedure Attach_Transform is
      Log       : Tasked_Logger.Instance (Init => False);
      Transform : constant Transforms.Handle := new Transforms.Casing.Instance;
      Count     : Natural := Natural'Last;
   begin
      Log.Transform_Count (Count => Count);

      Assert (Condition => Count = 0,
              Message   => "transform count not 0");

      Log.Attach_Transform (Transform => Transform);
      Log.Transform_Count (Count => Count);
      Assert (Condition => Count = 1,
              Message => "could not attach transform");

      begin
         Log.Attach_Transform (Transform => Transform);
         Fail (Message => "attached duplicate transform");

      exception
         when Logger.Transform_Already_Present =>
            null;
      end;
   end Attach_Transform;

   -------------------------------------------------------------------------

   procedure Default_Facility_Handling is
      Logger1 : Tasked_Logger.Instance (Init => False);
      Logger2 : Tasked_Logger.Instance (Init => True);
      F_Count : Natural := Natural'Last;
   begin
      Logger1.Attach_Default_Facility;
      Logger1.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 1,
              Message   => "Unable to attach facility");

      Logger1.Attach_Default_Facility;
      Logger1.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 1,
              Message   => "Attached facility twice");

      Logger1.Detach_Default_Facility;
      Logger1.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 0,
              Message   => "Unable to detach facility");

      Logger2.Attach_Default_Facility;
      Logger2.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 1,
              Message   => "Attached facility to initialzed logger");

      Logger2.Detach_Default_Facility;
      Logger2.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 0,
              Message   => "Unable to detach facility from logger");
   end Default_Facility_Handling;

   -------------------------------------------------------------------------

   procedure Detach_Facility_Unattached is
      Log      : Tasked_Logger.Instance (Init => False);
      Facility : Facilities.Handle :=
        new Facilities.Syslog.Instance;
   begin
      begin
         Facility.Set_Name ("Syslog_Facility");
         Log.Detach_Facility (Name => Facility.Get_Name);
         Fail (Message => "could detach unattached facility");

      exception
         when Logger.Facility_Not_Found =>

            --  Free not attached facility, this is not done by the logger
            --  (since it was never attached).

            Alog.Logger.Free (Facility);
      end;

      declare
         F_Count : Natural := Natural'Last;
      begin

         --  Tasking_Error will be raised if tasked logger has terminated due
         --  to an unhandled exception.

         Log.Facility_Count (Count => F_Count);

      end;

   end Detach_Facility_Unattached;

   -------------------------------------------------------------------------

   procedure Detach_Transform is
      Log       : Tasked_Logger.Instance (Init => False);
      Transform : constant Transforms.Handle := new Transforms.Casing.Instance;
      Count     : Natural := 0;
   begin
      Transform.Set_Name ("Casing_Transform");
      Log.Attach_Transform (Transform => Transform);
      Log.Transform_Count (Count => Count);
      Assert (Condition => Count = 1,
              Message   => "Unable to attach transform");

      Log.Detach_Transform (Name => Transform.Get_Name);
      Log.Transform_Count (Count => Count);
      Assert (Condition => Count = 0,
              Message   => "Unable to detach transform");
   end Detach_Transform;

   -------------------------------------------------------------------------

   procedure Detach_Transform_Unattached is
      Log       : Tasked_Logger.Instance (Init => False);
      Transform : Transforms.Handle :=
        new Transforms.Casing.Instance;
   begin
      begin
         Transform.Set_Name ("Casing_Transform");
         Log.Detach_Transform (Name => Transform.Get_Name);
         Fail (Message => "could detach unattached transform");

      exception
         when Logger.Transform_Not_Found =>
            --  Free not attached Transform, this is not done by the logger
            --  (since it was never attached).
            Alog.Logger.Free (Transform);
      end;

      declare
         T_Count : Natural := Natural'Last;
      begin

         --  Tasking_Error will be raised if tasked logger has terminated due
         --  to an unhandled exception.

         Log.Transform_Count (Count => T_Count);

      end;

   end Detach_Transform_Unattached;

   -------------------------------------------------------------------------

   procedure Enable_Facility_Timestamp
     (Facility_Handle : Facilities.Handle)
   is
   begin
      Facility_Handle.Toggle_Write_Timestamp (State => True);
   end Enable_Facility_Timestamp;

   -------------------------------------------------------------------------

   procedure Except_Handler
     (Except : Ada.Exceptions.Exception_Occurrence;
      Caller : Ada.Task_Identification.Task_Id)
   is
      pragma Unreferenced (Except, Caller);
   begin
      Counter := Counter + 1;
   end Except_Handler;

   -------------------------------------------------------------------------

   procedure Inc_Counter (F_Handle : Facilities.Handle)
   is
      pragma Unreferenced (F_Handle);
   begin
      Counter := Counter + 1;
   end Inc_Counter;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for tasked Logger");
      T.Add_Test_Routine
        (Routine => Attach_Facility'Access,
         Name    => "attach facility");
      T.Add_Test_Routine
        (Routine => Update_Facility'Access,
         Name    => "update a facility");
      T.Add_Test_Routine
        (Routine => Detach_Facility_Unattached'Access,
         Name    => "detach not attached facility");
      T.Add_Test_Routine
        (Routine => Attach_Transform'Access,
         Name    => "attach a transform");
      T.Add_Test_Routine
        (Routine => Detach_Transform'Access,
         Name    => "detach transform");
      T.Add_Test_Routine
        (Routine => Detach_Transform_Unattached'Access,
         Name    => "detach not attached transform");
      T.Add_Test_Routine
        (Routine => Verify_Logger_Initialization'Access,
         Name    => "logger initialization behavior");
      T.Add_Test_Routine
        (Routine => Logger_Exception_Handling'Access,
         Name    => "exception handling");
      T.Add_Test_Routine
        (Routine => Default_Facility_Handling'Access,
         Name    => "default facility handling");
      T.Add_Test_Routine
        (Routine => Iterate_Facilities'Access,
         Name    => "iterate over facilities");
      T.Add_Test_Routine
        (Routine => Iterate_Facilities_Exceptions'Access,
         Name    => "iterate facilities exceptions");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Iterate_Facilities is
      Log       : Tasked_Logger.Instance (Init => False);

      Facility1 : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Facility2 : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
   begin
      Counter := 0;

      Facility1.Set_Name (Name => "Facility1");
      Facility2.Set_Name (Name => "Facility2");

      Log.Attach_Facility (Facility => Facility1);
      Log.Attach_Facility (Facility => Facility2);

      Log.Iterate (Process => Inc_Counter'Access);

      Log.Clear;
      Assert (Condition => Counter = 2,
              Message   => "counter not 2");
   end Iterate_Facilities;

   -------------------------------------------------------------------------

   procedure Iterate_Facilities_Exceptions is
      use Ada.Exceptions;

      Log       : Tasked_Logger.Instance (Init => False);
      Facility1 : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
   begin
      Counter := 0;
      Log.Set_Except_Handler (Proc => Except_Handler'Access);
      Facility1.Set_Name (Name => "Facility1");

      Log.Attach_Facility (Facility => Facility1);
      Log.Iterate (Process => Raise_Exception'Access);

      for I in 1 .. 30 loop
         exit when Counter /= 0;
         delay 0.1;
      end loop;

      Assert (Condition => Counter = 1,
              Message   => "Exception counter not 1");

      Log.Clear;
   end Iterate_Facilities_Exceptions;

   -------------------------------------------------------------------------

   procedure Logger_Exception_Handling is
      Log           : Tasked_Logger.Instance;
      Mock_Facility : constant Facilities.Handle :=
        new Facilities.Mock.Instance;
   begin
      Counter := 0;
      Log.Set_Except_Handler (Proc => Except_Handler'Access);
      Log.Attach_Facility (Facility => Mock_Facility);
      Log.Log_Message (Level => Debug,
                       Msg   => "Test message");

      for I in 1 .. 30 loop
         exit when Counter /= 0;
         delay 0.1;
      end loop;

      Assert (Condition => Counter = 1,
              Message   => "Exception counter not 1");
   end Logger_Exception_Handling;

   -------------------------------------------------------------------------

   procedure Raise_Exception (F_Handle : Facilities.Handle) is
   begin
      raise Constraint_Error with "DON'T PANIC! This is a test exception!";
   end Raise_Exception;

   -------------------------------------------------------------------------

   procedure Update_Facility is
      Log : Tasked_Logger.Instance (Init => False);
   begin
      Counter := 0;
      Log.Set_Except_Handler (Proc => Except_Handler'Access);
      Log.Update (Name    => "Nonexistent",
                  Process => Do_Nothing'Access);

      for I in 1 .. 30 loop
         exit when Counter /= 0;
         delay 0.1;
      end loop;

      Assert (Condition => Counter = 1,
              Message   => "Exception counter not 1");

      declare
         Facility      : constant Facilities.Handle :=
           new Facilities.File_Descriptor.Instance;
         Facility_Name : constant String            :=
           "Test_Facility";
      begin
         Facility.Set_Name (Name => Facility_Name);
         Facility.Toggle_Write_Timestamp (State => False);
         Assert (Condition => not Facility.Is_Write_Timestamp,
                 Message   => "Could not disable Timestamp");

         Log.Attach_Facility (Facility => Facility);
         Log.Update (Name    => Facility_Name,
                     Process => Enable_Facility_Timestamp'Access);

         --  Since Update is not synchronous and we are accessing the facility
         --  directly we must wait for the update to actually take place.

         delay 0.1;

         Assert (Condition => Facility.Is_Write_Timestamp,
                 Message   => "Update failed");
      end;
   end Update_Facility;

   -------------------------------------------------------------------------

   procedure Verify_Logger_Initialization is
      Logger1 : Tasked_Logger.Instance;
      Logger2 : Tasked_Logger.Instance (Init => True);
      F_Count : Natural := Natural'Last;
   begin

      Logger1.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 0,
              Message   => "logger1 not empty");

      Logger2.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 1,
              Message   => "logger2 empty");
   end Verify_Logger_Initialization;

end Tasked_Logger_Tests;
