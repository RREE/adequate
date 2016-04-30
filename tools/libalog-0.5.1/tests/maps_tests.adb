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

with Ahven; use Ahven;

with Alog.Maps;

package body Maps_Tests is

   use Alog;

   -------------------------------------------------------------------------

   procedure Clear_Map is
      Map : Maps.Wildcard_Level_Map;
   begin
      Map.Insert (Key  => "Foo",
                  Item => Debug);
      Map.Insert (Key  => "Bar",
                  Item => Warning);
      Assert (Condition => Map.Length = 2,
              Message   => "Count not 2");

      Map.Clear;
      Assert (Condition => Map.Length = 0,
              Message   => "Count not 0");
   end Clear_Map;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for maps");
      T.Add_Test_Routine
        (Routine => Insert_Elements'Access,
         Name    => "Insert elements");
      T.Add_Test_Routine
        (Routine => Update_Element'Access,
         Name    => "Update element");
      T.Add_Test_Routine
        (Routine => Wildcard_Lookup'Access,
         Name    => "Wildcard lookup");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Insert_Elements is
      use type Maps.Cursor;

      Map : Maps.Wildcard_Level_Map;
   begin
      Map.Insert (Key  => "Foo",
                  Item => Debug);

      Assert (Condition => Map.Length = 1,
              Message   => "Count not 1");
      Assert (Condition => Map.Element (Key => "Foo") = Debug,
              Message   => "Loglevel mismatch for key Foo");

      declare
         Position : Maps.Cursor;
      begin
         Position := Map.Find (Key => "Foo");

         Assert (Condition => Position /= Maps.No_Element,
                 Message   => "No element for key Foo");

         Assert (Condition => Maps.Element (Position => Position) = Debug,
                 Message   => "Loglevel mismatch for position");
      end;

      declare
         Position : Maps.Cursor;
      begin
         Position := Map.Find (Key => "Bar");

         Assert (Condition => Position = Maps.No_Element,
                 Message   => "Found element for nonexistent key");
      end;

      declare
         Level : Log_Level;
         pragma Unreferenced (Level);
      begin
         Level := Maps.Element (Position => Maps.No_Element);
         Fail (Message => "Expected constraint error");

      exception
         when Constraint_Error =>
            null;
      end;
   end Insert_Elements;

   -------------------------------------------------------------------------

   procedure Update_Element is
      Map : Maps.Wildcard_Level_Map;
   begin
      Map.Insert (Key  => "Foo",
                  Item => Debug);
      Assert (Condition => Map.Element (Key => "Foo") = Debug,
              Message   => "Loglevel mismatch");

      Map.Insert (Key  => "Foo",
                  Item => Error);
      Assert (Condition => Map.Element (Key => "Foo") = Error,
              Message   => "Update failed");
   end Update_Element;

   -------------------------------------------------------------------------

   procedure Wildcard_Lookup is
      Map : Maps.Wildcard_Level_Map;
   begin
      Map.Insert (Key  => "Foo.*",
                  Item => Notice);
      Map.Insert (Key  => "Foo.Bar",
                  Item => Warning);

      declare
         Position : Maps.Cursor;
      begin
         Position := Map.Lookup (Key => "Foo");
         Assert (Condition => Maps.Element (Position => Position) = Notice,
                 Message   => "Loglevel for Foo not notice");
      end;

      declare
         Position : Maps.Cursor;
      begin
         Position := Map.Lookup (Key => "Foo.Bar");
         Assert (Condition => Maps.Element (Position => Position) = Warning,
                 Message   => "Loglevel for Foo.Bar not warning");
      end;

      declare
         Position : Maps.Cursor;
      begin
         Position := Map.Lookup (Key => "Foo.Foo");
         Assert (Condition => Maps.Element (Position => Position) = Notice,
                 Message   => "Loglevel for Foo.Foo not notice");
      end;

      declare
         Position : Maps.Cursor;
      begin
         Position := Map.Lookup (Key => "Foo.Bar.Foo");
         Assert (Condition => Maps.Element (Position => Position) = Notice,
                 Message   => "Loglevel for Foo.Bar.Foo not notice");
      end;

      declare
         use type Maps.Cursor;

         Position : Maps.Cursor;
      begin
         Position := Map.Lookup (Key => "Bar.Bar");
         Assert (Condition => Position = Maps.No_Element,
                 Message   => "No_Element expected");
      end;
   end Wildcard_Lookup;

end Maps_Tests;
