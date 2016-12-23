--
--  Copyright (c) 2008,
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

with Alog.Transforms.Casing;

package body Transform_Tests is

   use Alog;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for Transforms");
      T.Add_Test_Routine
        (Routine => Set_Name'Access,
         Name    => "set transform name");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Set_Name is
      T        : Transforms.Casing.Instance;
      Expected : constant String := "Test transform";
   begin
      T.Set_Name (Name => Expected);
      Assert (Condition => T.Get_Name = Expected,
              Message => "name not equal");
   end Set_Name;

end Transform_Tests;
