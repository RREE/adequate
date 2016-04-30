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

--  Tests for Alog tasked logger component.
package Tasked_Logger_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Attach_Facility;
   --  Test Facility attaching to tasked logger.

   procedure Update_Facility;
   --  Test Facility update operation.

   procedure Detach_Facility_Unattached;
   --  Test Facility detaching of tasked logger with a un-attached instance.
   --  Verify that logger remains responsive after exception.

   procedure Attach_Transform;
   --  Test Transform attaching to tasked logger.

   procedure Detach_Transform;
   --  Test Transform detaching of tasked logger.

   procedure Detach_Transform_Unattached;
   --  Test Transform detaching of tasked logger with un-attached instance.
   --  Verify  that logger remains responsive after exception.

   procedure Verify_Logger_Initialization;
   --  Test tasked logger instance initialization behavior.

   procedure Logger_Exception_Handling;
   --  Test tasked logger instance exception handling.

   procedure Default_Facility_Handling;
   --  Test tasked logger attaching/detaching of default facility.

   procedure Iterate_Facilities;
   --  Test tasked logger facility iteration.

   procedure Iterate_Facilities_Exceptions;
   --  Test tasked logger facility iteration with exceptions.

end Tasked_Logger_Tests;
