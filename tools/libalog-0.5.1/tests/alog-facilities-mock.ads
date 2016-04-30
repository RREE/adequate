--
--  Copyright (c) 2009,
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

--  Logging facility used for testing. Raises a Constraint_Error when Write()
--  is called.
package Alog.Facilities.Mock is

   type Instance is new Alog.Facilities.Instance with private;

   type Handle is access all Instance;

   overriding
   procedure Write
     (Facility : Instance;
      Level    : Log_Level := Info;
      Msg      : String);
   --  Implementation of Write.

   overriding
   procedure Setup (Facility : in out Instance) is null;
   --  Implementation of Setup procedure.

   overriding
   procedure Teardown (Facility : in out Instance) is null;
   --  Implementation of Teardown procedure.

   Exception_Message : constant String := "DON'T PANIC! Test exception";

private

   type Instance is new Alog.Facilities.Instance with null record;

end Alog.Facilities.Mock;
