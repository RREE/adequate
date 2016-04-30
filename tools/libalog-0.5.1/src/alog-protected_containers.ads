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

with Ada.Containers.Doubly_Linked_Lists;

with Alog.Log_Request;

--  Alog Protected Containers. This package provides protected containers which
--  are safe for concurrent access.
package Alog.Protected_Containers is

   ----------------------
   -- Log_Request_List --
   ----------------------

   type Log_Request_Storage is private;

   protected type Log_Request_List is

      procedure Put (Element : Log_Request.Instance);
      --  Put an element at the end of the request list.

      entry Get (Element : out Log_Request.Instance);
      --  Get the first element from the list (and delete it).

      procedure Done;
      --  Signal successfull processing of request previously gotten from list.

      entry All_Done;
      --  This procedure blocks until the list is empty and there are no
      --  pending requests. A requests is pending when it is taken off the list
      --  via Get but it's successfull processing has not been signaled back
      --  via the procedure Done.

      procedure Clear;
      --  Clear the request list by deleting all log requests.

      function Length return Natural;
      --  Return the number of elements in the list.

      function Pending return Natural;
      --  Return the number of pending requests.

   private

      Requests           : Log_Request_Storage;
      Requests_Available : Boolean := False;
      Pending_Counter    : Natural := 0;

   end Log_Request_List;
   --  Protected variant of a log request list. This list holds log request
   --  objects and is safe for concurrent access. It operates in FIFO-Mode.

private

   use type Alog.Log_Request.Instance;

   package List_Of_Log_Requests_Package is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type => Log_Request.Instance);

   package LOLRP renames List_Of_Log_Requests_Package;

   type Log_Request_Storage is new LOLRP.List with null record;

end Alog.Protected_Containers;
