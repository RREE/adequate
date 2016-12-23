--
--  Copyright (c) 2011,
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

with Ada.Exceptions;
with Ada.Task_Identification;

--  Alog exception package. Contains type definitions for Alog specific
--  exception handling.
package Alog.Exceptions is

   type Exception_Handler is not null access procedure
     (Except : Ada.Exceptions.Exception_Occurrence;
      Caller : Ada.Task_Identification.Task_Id);
   --  Exception handler callback procedure. Used to register custom callback
   --  procedures in active and tasked loggers.

end Alog.Exceptions;
