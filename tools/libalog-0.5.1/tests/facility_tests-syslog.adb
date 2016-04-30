--
--  Copyright (c) 2011,
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

with Ahven;

with Alog.Facilities.Syslog;

package body Facility_Tests.Syslog is

   use Ahven;

   package FS renames Alog.Facilities.Syslog;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for syslog Facility");
      T.Add_Test_Routine
        (Routine => Set_Syslog_Origin'Access,
         Name    => "set/get origin");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Set_Syslog_Origin
   is
      use type Alog.Facilities.Syslog.Syslog_Origin;

      F : FS.Instance;
   begin
      Assert (Condition => F.Get_Origin = FS.LOG_USER,
              Message   => "Default origin not LOG_USER");

      F.Set_Origin (Value => FS.LOG_MAIL);
      Assert (Condition => F.Get_Origin = FS.LOG_MAIL,
              Message   => "Origin not LOG_MAIL");
   end Set_Syslog_Origin;

end Facility_Tests.Syslog;
