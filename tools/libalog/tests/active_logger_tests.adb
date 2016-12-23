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

with Ada.Directories;

with Ahven;

with Alog.Helpers;
with Alog.Facilities.File_Descriptor;
with Alog.Facilities.Syslog;
with Alog.Transforms.Casing;
with Alog.Logger;
with Alog.Active_Logger;

package body Active_Logger_Tests is

   use Ahven;
   use Alog;

   Ref_Facility_Name : constant String := "Test_Facility_Name";

   Counter : Natural := 0;
   pragma Atomic (Counter);

   procedure Check_Facility (Facility_Handle : Facilities.Handle);
   --  Verify that facility with given name is present in the logger.

   procedure Inc_Counter (F_Handle : Facilities.Handle);
   --  Increment counter.

   procedure Toggle_Timestamp (Facility_Handle : Facilities.Handle);
   --  Helper function for update facility test: Set the facility's write
   --  timestamp flag to True.

   -------------------------------------------------------------------------

   procedure Attach_Facility
   is
      Log      : aliased Active_Logger.Instance (Init => False);
      Facility : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
   begin
      Facility.Set_Name (Name => Ref_Facility_Name);

      declare
         Shutdown : Active_Logger.Shutdown_Helper (Logger => Log'Access);
         pragma Unreferenced (Shutdown);
      begin
         Assert (Condition => Log.Facility_Count = 0,
                 Message   => "Facility count not 0");

         Log.Attach_Facility (Facility => Facility);
         Assert (Condition => Log.Facility_Count = 1,
                 Message => "Could not attach facility");

         Log.Update (Name    => Facility.Get_Name,
                     Process => Check_Facility'Access);

         begin
            Log.Attach_Facility (Facility => Facility);
            Fail (Message => "Attached duplicate facility");

         exception
            when Logger.Facility_Already_Present => null;
         end;

      end;
   end Attach_Facility;

   -------------------------------------------------------------------------

   procedure Attach_Transform is
      Log       : aliased Active_Logger.Instance (Init => False);
      Transform : constant Transforms.Handle := new Transforms.Casing.Instance;
   begin
      declare
         Shutdown : Active_Logger.Shutdown_Helper (Logger => Log'Access);
         pragma Unreferenced (Shutdown);
      begin
         Assert (Condition => Log.Transform_Count = 0,
                 Message   => "transform count not 0");

         Log.Attach_Transform (Transform => Transform);
         Assert (Condition => Log.Transform_Count = 1,
                 Message => "could not attach transform");

         begin
            Log.Attach_Transform (Transform => Transform);

            Fail (Message => "attached duplicate transform");

         exception
            when Logger.Transform_Already_Present =>
               null;
         end;
      end;
   end Attach_Transform;

   -------------------------------------------------------------------------

   procedure Check_Facility (Facility_Handle : Facilities.Handle) is
      use type Facilities.Handle;
   begin
      Assert (Condition => Facility_Handle.Get_Name = Ref_Facility_Name,
              Message   => "facility name mismatch");
   end Check_Facility;

   -------------------------------------------------------------------------

   procedure Clear_A_Logger is
      Log       : aliased Active_Logger.Instance (Init => False);
      Facility  : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Transform : constant Transforms.Handle :=
        new Transforms.Casing.Instance;
   begin
      declare
         Shutdown : Active_Logger.Shutdown_Helper (Logger => Log'Access);
         pragma Unreferenced (Shutdown);
      begin
         Log.Attach_Facility (Facility => Facility);
         Assert (Condition => Log.Facility_Count = 1,
                 Message   => "could not attach facility");

         Log.Attach_Transform (Transform => Transform);
         Assert (Condition => Log.Transform_Count = 1,
                 Message   => "could not attach transform");

         Log.Clear;
         Assert (Condition => Log.Facility_Count = 0,
                 Message   => "facility count is not 0");
         Assert (Condition => Log.Transform_Count = 0,
                 Message   => "transform count is not 0");
      end;
   end Clear_A_Logger;

   -------------------------------------------------------------------------

   procedure Default_Facility_Handling is
      Logger1 : aliased Active_Logger.Instance (Init => False);
      Logger2 : aliased Active_Logger.Instance (Init => True);
   begin
      declare
         Shutdown1 : Active_Logger.Shutdown_Helper (Logger => Logger1'Access);
         Shutdown2 : Active_Logger.Shutdown_Helper (Logger => Logger2'Access);
         pragma Unreferenced (Shutdown1);
         pragma Unreferenced (Shutdown2);
      begin
         Logger1.Attach_Default_Facility;
         Assert (Condition => Logger1.Facility_Count = 1,
                 Message   => "Unable to attach facility");
         Logger1.Attach_Default_Facility;
         Assert (Condition => Logger1.Facility_Count = 1,
                 Message   => "Attached facility twice");

         Logger1.Log_Message (Level => Debug,
                              Msg => "Testing default logger");

         Logger1.Detach_Default_Facility;
         Assert (Condition => Logger1.Facility_Count = 0,
                 Message   => "Unable to detach facility");

         Logger2.Attach_Default_Facility;
         Assert (Condition => Logger2.Facility_Count = 1,
                 Message   => "Attached facility to initialzed logger");
         Logger2.Detach_Default_Facility;
         Assert (Condition => Logger2.Facility_Count = 0,
                 Message   => "Unable to detach facility from initialized"
                 & " logger");
      end;
   end Default_Facility_Handling;

   -------------------------------------------------------------------------

   procedure Detach_Facility_Instance is
      Log      : aliased Active_Logger.Instance (Init => False);
      Facility : constant Facilities.Handle :=
        new Facilities.Syslog.Instance;
   begin
      declare
         Shutdown : Active_Logger.Shutdown_Helper (Logger => Log'Access);
         pragma Unreferenced (Shutdown);
      begin
         Facility.Set_Name ("Syslog_Facility");
         Log.Attach_Facility (Facility => Facility);
         Assert (Condition => Log.Facility_Count = 1,
                 Message   => "could not attach");
         Log.Detach_Facility (Name => Facility.Get_Name);
         Assert (Condition => Log.Facility_Count = 0,
                 Message   => "could not detach");
      end;
   end Detach_Facility_Instance;

   -------------------------------------------------------------------------

   procedure Detach_Facility_Unattached is
      Log      : aliased Active_Logger.Instance (Init => False);
      Facility : Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
   begin
      declare
         Shutdown : Active_Logger.Shutdown_Helper (Logger => Log'Access);
         pragma Unreferenced (Shutdown);
      begin
         Log.Detach_Facility (Name => Facility.Get_Name);
         Fail (Message => "could detach unattached facility");

      exception
         when Logger.Facility_Not_Found =>
            --  Free not attached facility, this is not done by the logger
            --  (since it was never attached).
            Alog.Logger.Free (Facility);
            --  Test passed.
      end;
   end Detach_Facility_Unattached;

   -------------------------------------------------------------------------

   procedure Detach_Transform_Instance is
      Log       : aliased Active_Logger.Instance (Init => False);
      Transform : constant Transforms.Handle := new Transforms.Casing.Instance;
   begin
      declare
         Shutdown : Active_Logger.Shutdown_Helper (Logger => Log'Access);
         pragma Unreferenced (Shutdown);
      begin
         Transform.Set_Name ("Casing_Transform");
         Log.Attach_Transform (Transform => Transform);
         Assert (Condition => Log.Transform_Count = 1,
                 Message   => "could not attach");
         Log.Detach_Transform (Name => Transform.Get_Name);
         Assert (Condition => Log.Transform_Count = 0,
                 Message   => "could not detach");
      end;
   end Detach_Transform_Instance;

   -------------------------------------------------------------------------

   procedure Detach_Transform_Unattached is
      Log       : aliased Active_Logger.Instance (Init => False);
      Transform : Transforms.Handle :=
        new Transforms.Casing.Instance;
   begin
      declare
         Shutdown : Active_Logger.Shutdown_Helper (Logger => Log'Access);
         pragma Unreferenced (Shutdown);
      begin
         Transform.Set_Name ("Casing_Transform");
         Log.Detach_Transform (Name => Transform.Get_Name);
         Fail (Message => "could detach unattached transform");

      exception
         when Logger.Transform_Not_Found =>
            --  Free not attached Transform, this is not done by the logger
            --  (since it was never attached).
            Alog.Logger.Free (Transform);
            --  Test passed.
      end;
   end Detach_Transform_Unattached;

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
      T.Set_Name (Name => "Tests for active Logger");
      T.Add_Test_Routine
        (Routine => Attach_Facility'Access,
         Name    => "attach a facility");
      T.Add_Test_Routine
        (Routine => Update_Facility'Access,
         Name    => "update a facility");
      T.Add_Test_Routine
        (Routine => Detach_Facility_Instance'Access,
         Name    => "detach facility: instance");
      T.Add_Test_Routine
        (Routine => Detach_Facility_Unattached'Access,
         Name    => "detach not attached facility");
      T.Add_Test_Routine
        (Routine => Attach_Transform'Access,
         Name    => "attach a transform");
      T.Add_Test_Routine
        (Routine => Detach_Transform_Instance'Access,
         Name    => "detach a transform");
      T.Add_Test_Routine
        (Routine => Detach_Transform_Unattached'Access,
         Name    => "detach not attached transform");
      T.Add_Test_Routine
        (Routine => Clear_A_Logger'Access,
         Name    => "clear logger");
      T.Add_Test_Routine
        (Routine => Verify_Logger_Initialization'Access,
         Name    => "logger initialization behavior");
      T.Add_Test_Routine
        (Routine => Default_Facility_Handling'Access,
         Name    => "default facility handling");
      T.Add_Test_Routine
        (Routine => Tasked_One_FD_Facility'Access,
         Name    => "log with active logger");
      T.Add_Test_Routine
        (Routine => Verify_Iterate_Facilities'Access,
         Name    => "verify iterate facilities");
      T.Add_Test_Routine
        (Routine => Task_Termination'Access,
         Name    => "task termination");
      T.Add_Test_Routine
        (Routine => Request_Queue_Length'Access,
         Name    => "request queue length");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Request_Queue_Length is
      Log : aliased Active_Logger.Instance (Init => False);
   begin
      declare
         Shutdown : Active_Logger.Shutdown_Helper (Logger => Log'Access);
         pragma Unreferenced (Shutdown);
      begin
         Assert (Condition => Log.Get_Queue_Length = 0,
                 Message   => "Queue not empty");
      end;
   end Request_Queue_Length;

   -------------------------------------------------------------------------

   procedure Task_Termination is
      Log : aliased Active_Logger.Instance (Init => False);
   begin
      declare
         Shutdown : Active_Logger.Shutdown_Helper (Logger => Log'Access);
         pragma Unreferenced (Shutdown);
      begin
         Assert (Condition => Log.Is_Terminated = False,
                 Message   => "New Logger terminated");

         declare
            Counter : Natural := 0;
         begin
            Log.Shutdown;

            while not Log.Is_Terminated loop
               delay 0.01;
               Counter := Counter + 1;
               if Counter = 200 then
                  Fail (Message => "Logger still running");
               end if;
            end loop;
         end;
      end;
   end Task_Termination;

   -------------------------------------------------------------------------

   procedure Tasked_One_FD_Facility is
      Log            : aliased Active_Logger.Instance (Init => False);
      Fd_Facility1   : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Fd_Facility2   : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;

      Testfile       : constant String  :=
        "./data/Active_Tasked_FD_Facility";
      Reffile        : constant String  :=
        "./data/Tasked_FD_Facility.ref";
      Test_Message   : constant String  := "logger tasked test message";
      Nr_Of_Messages : constant Natural := 10;

      task type Test_Log_Task
        (Logger : not null access Active_Logger.Instance);
      --  This task logs Nr_Of_Messages to the given Active logger instance.

      task body Test_Log_Task is
      begin
         for I in 1 .. Nr_Of_Messages loop
            Logger.Log_Message (Level  => Debug,
                                Msg    => Test_Message);
         end loop;
      end Test_Log_Task;

   begin
      declare
         Shutdown : Active_Logger.Shutdown_Helper (Logger => Log'Access);
         pragma Unreferenced (Shutdown);
      begin
         Fd_Facility1.Set_Name (Name => "Fd_Facility1");
         Fd_Facility1.Toggle_Write_Timestamp (State => False);

         Facilities.File_Descriptor.Handle
           (Fd_Facility1).Set_Logfile (Path => Testfile);

         Log.Attach_Facility (Facility => Fd_Facility1);
         Log.Attach_Facility (Facility => Fd_Facility2);

         Assert (Condition => Log.Facility_Count = 2,
                 Message   => "facility count not 2");

         Log.Detach_Facility (Name => Fd_Facility2.Get_Name);
         Assert (Condition => Log.Facility_Count = 1,
                 Message   => "facility count not 1");

         declare
            Logger1 : Test_Log_Task (Log'Access);
         begin
            for I in 1 .. Nr_Of_Messages loop
               Log.Log_Message (Level  => Debug,
                                Msg    => Test_Message);
            end loop;
         end;
      end;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "files not equal");

      Ada.Directories.Delete_File (Name => Testfile);
   end Tasked_One_FD_Facility;

   -------------------------------------------------------------------------

   procedure Toggle_Timestamp (Facility_Handle : Facilities.Handle)
   is
   begin
      Facility_Handle.Toggle_Write_Timestamp (State => True);
   end Toggle_Timestamp;

   -------------------------------------------------------------------------

   procedure Update_Facility is
      Log : aliased Active_Logger.Instance (Init => False);
   begin
      declare
         Shutdown : Active_Logger.Shutdown_Helper (Logger => Log'Access);
         pragma Unreferenced (Shutdown);
      begin
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
                        Process => Toggle_Timestamp'Access);

            for I in 1 .. 30 loop
               exit when Facility.Is_Write_Timestamp;
               delay 0.1;
            end loop;

            Assert (Condition => Facility.Is_Write_Timestamp,
                    Message   => "Update failed");
         end;
      end;
   end Update_Facility;

   -------------------------------------------------------------------------

   procedure Verify_Iterate_Facilities is
      Log       : aliased Active_Logger.Instance (Init => False);
      Facility1 : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Facility2 : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
   begin
      declare
         Shutdown : Active_Logger.Shutdown_Helper (Logger => Log'Access);
         pragma Unreferenced (Shutdown);
      begin
         Facility1.Set_Name (Name => "Facility1");
         Facility1.Set_Name (Name => "Facility2");

         Log.Attach_Facility (Facility => Facility1);
         Log.Attach_Facility (Facility => Facility2);

         Log.Iterate (Process => Inc_Counter'Access);

         for I in 1 .. 30 loop
            exit when Counter = 2;
            delay 0.1;
         end loop;

         Assert (Condition => Counter = 2,
                 Message   => "Counter not 2:" & Counter'Img);
      end;
   end Verify_Iterate_Facilities;

   -------------------------------------------------------------------------

   procedure Verify_Logger_Initialization is
      Logger1 : aliased Active_Logger.Instance (Init => False);
      Logger2 : aliased Active_Logger.Instance (Init => True);
   begin
      declare
         Shutdown1 : Active_Logger.Shutdown_Helper (Logger => Logger1'Access);
         Shutdown2 : Active_Logger.Shutdown_Helper (Logger => Logger2'Access);
         pragma Unreferenced (Shutdown1);
         pragma Unreferenced (Shutdown2);
      begin
         Assert (Condition => Logger1.Facility_Count = 0,
                 Message   => "logger1 not empty");
         Assert (Condition => Logger2.Facility_Count = 1,
                 Message   => "logger2 empty");
      end;
   end Verify_Logger_Initialization;

end Active_Logger_Tests;
