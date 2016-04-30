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
--

with Ahven.Framework;

--  Tests for the logging policy database package.
package Policy_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Finalize (T : in out Testcase);
   --  Finalize testcase.

   procedure Reset_Policy_DB;
   --  Reset the policy database to initial state.

   procedure Default_Loglevel_Handling;
   --  Test default loglevel handling.

   procedure Ident_Loglevel_Handling;
   --  Test setting/getting of identifier based loglevels.

   procedure Set_Identifier_Map;
   --  Test setting of identifier based loglevels with wildcard map.

   procedure Verify_Accept_Src;
   --  Verify accept src behavior.

   procedure Verify_Accept_Dst;
   --  Verify accept dst behavior.

   procedure Lookup_Ident;
   --  Verify identifier lookup behavior.

end Policy_Tests;
