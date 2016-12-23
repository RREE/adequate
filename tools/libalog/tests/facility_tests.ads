--
--  Copyright (c) 2008-2009,
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

with Ahven.Framework;

--  Tests for Alog facility components.
package Facility_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Set_Name;
   --  Set name of Facility test.

   procedure Toggle_Loglevel;
   --  Test loglevel-writing related procedures.

   procedure Toggle_Timestamp;
   --  Test timestamp-writing related procedures.

   procedure Toggle_UTC_Timestamp;
   --  Test enabling/disabling UTC timestamps.

   procedure Toggle_Source;
   --  Test enabling/disabling source writing.

   procedure Timestamp_Creation;
   --  Test timestamp string creation.

   procedure Timestamp_Format_Setter;
   --  Test setting of timestamp formats.

end Facility_Tests;
