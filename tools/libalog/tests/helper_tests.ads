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

--  Tests for Alog helpers.
package Helper_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Compare_Files;
   --  Verify Assert_Files_Equal helper function.

   procedure Read_Config;
   --  Verify configuration file loading.

   procedure Read_Config_Nodefault;
   --  Read a loglevel config file without default loglevel line.

   procedure Read_Config_Invalid_Loglevel;
   --  Try to read a loglevel config file with invalid loglevel.

   procedure Read_Invalid_Config;
   --  Try to read an invalid config file.

end Helper_Tests;
