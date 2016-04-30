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

package body Alog.Transforms is

   -------------------------------------------------------------------------

   function "="
     (Left  : Handle;
      Right : Handle)
      return Boolean is
   begin
      return Left.Get_Name = Right.Get_Name;
   end "=";

   -------------------------------------------------------------------------

   function Get_Name (Transform : Class) return String is
   begin
      return To_String (Transform.Name);
   end Get_Name;

   -------------------------------------------------------------------------

   procedure Set_Name
     (Transform : in out Class;
      Name      :        String)
   is
   begin
      Transform.Name := To_Unbounded_String (Name);
   end Set_Name;

end Alog.Transforms;
