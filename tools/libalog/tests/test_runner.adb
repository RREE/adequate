--
--  Copyright (c) 2008-2014,
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

with Ahven.Text_Runner;
with Ahven.Framework;

with Helper_Tests;
with Log_Request_Tests;
with Protected_Container_Tests;
with Facility_Tests.FD;
with Facility_Tests.Syslog;
with Transform_Tests.Casing;
with Logger_Tests;
with Tasked_Logger_Tests;
with Active_Logger_Tests;
with Maps_Tests;
with Policy_Tests;

procedure Test_Runner is
   S : constant Ahven.Framework.Test_Suite_Access :=
     Ahven.Framework.Create_Suite (Suite_Name => "Alog tests");
begin
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Helper_Tests.Testcase);
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Log_Request_Tests.Testcase);
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Protected_Container_Tests.Testcase);
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Maps_Tests.Testcase);
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Facility_Tests.Testcase);
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Facility_Tests.FD.Testcase);
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Facility_Tests.Syslog.Testcase);
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Transform_Tests.Testcase);
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Transform_Tests.Casing.Testcase);
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Logger_Tests.Testcase);
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Tasked_Logger_Tests.Testcase);
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Active_Logger_Tests.Testcase);
   Ahven.Framework.Add_Test (Suite => S.all,
                             T     => new Policy_Tests.Testcase);

   Ahven.Text_Runner.Run (Suite => S);
   Ahven.Framework.Release_Suite (T => S);
end Test_Runner;
