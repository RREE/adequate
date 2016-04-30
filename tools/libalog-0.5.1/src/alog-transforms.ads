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

with Ada.Strings.Unbounded;

--  Abstract package Transforms. Provides methods used by all Alog transforms.
package Alog.Transforms is

   use Ada.Strings.Unbounded;

   type Instance is abstract tagged limited private;
   --  Abstract type transform instance. All tranforms in the Alog framework
   --  must implement this type.

   subtype Class is Instance'Class;

   type Handle is access Class;

   function "="
     (Left  : Handle;
      Right : Handle)
      return Boolean;
   --  Equal function.

   procedure Set_Name
     (Transform : in out Class;
      Name      :        String);
   --  Set transform name.

   function Get_Name (Transform : Class) return String;
   --  Get transform name.

   function Transform_Message
     (Transform : Instance;
      Level     : Log_Level;
      Msg       : String)
      return String is abstract;
   --  Transform message with specified log level.

   procedure Setup (Transform : in out Instance) is null;
   --  Each transform must provide a Setup-procedure. These procedures are
   --  called by logger instances when attaching Transforms. All needed
   --  operations prior to transforming log messages should be done here.

   procedure Teardown (Transform : in out Instance) is null;
   --  Each transform must provide a Teardown-procedure. These procedures are
   --  called by logger instances when detaching Transforms or when the logger
   --  object gets out of scope.

private

   type Instance is abstract tagged limited record
      Name : Unbounded_String := To_Unbounded_String ("sample-transform");
      --  Transform Name. Names must be unique.
   end record;

end Alog.Transforms;
