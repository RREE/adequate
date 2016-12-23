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

with Ada.Finalization;
with Ada.Containers.Indefinite_Ordered_Maps;

generic
   type Key_Type (<>) is private;
   type Element_Type (<>) is limited private;
   type Element_Handle is access Element_Type;

   with function "<" (Left, Right : Key_Type) return Boolean is <>;

--  Controlled variant of a map. The memory of an element pointed to by a
--  previously inserted handle is freed upon calling Delete, Clear or during
--  finalization of the controlled map. Thus control over objects inserted into
--  this map resides with the controlled map.
package Alog.Controlled_Map is

   pragma Preelaborate;

   type Map is new Ada.Finalization.Limited_Controlled with private;
   --  A controlled map container.

   procedure Insert
     (Container : in out Map;
      Key       :        Key_Type;
      New_Item  :        Element_Handle);
   --  Insert a new element handle with 'Key' into the controlled map.

   function Element
     (Container : Map;
      Key       : Key_Type)
      return Element_Handle;
   --  Return a handle to an element identified by 'Key'.

   procedure Delete
     (Container : in out Map;
      Key       :        Key_Type);
   --  Delete the element with key 'Key' from the map. Memory of the element is
   --  freed.

   function Contains
     (Container : Map;
      Key       : Key_Type)
      return Boolean;
   --  Returns True if an element with key 'Key' is in the map.

   function Is_Empty (Container : Map) return Boolean;
   --  Returns True if the map is empty.

   procedure Clear (Container : in out Map);
   --  Remove all elements in the map. Memory of the elements is freed.

   function Length (Container : Map) return Natural;
   --  Return the current element count.

   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Handle : Element_Handle));
   --  Iterate over all elements in the map and call the 'Process' procedure
   --  for each handle.

private

   overriding
   procedure Finalize (Container : in out Map);
   --  Clean up the the controlled map. This will Free all the memory occupied
   --  by the elements in the map.

   package Map_Of_Elements_Package is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Key_Type,
        Element_Type => Element_Handle);

   package MOEP renames Map_Of_Elements_Package;

   type Map is new Ada.Finalization.Limited_Controlled with record
      Data : MOEP.Map;
   end record;

end Alog.Controlled_Map;
