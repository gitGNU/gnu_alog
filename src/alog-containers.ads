--
--  Copyright (c) 2009,
--  Reto Buerki, Adrian-Ken Rueegsegger
--  secunet SwissIT AG
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
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Alog.Log_Request;

--  Alog Containers. This package holds various container types used in Alog.
package Alog.Containers is

   ----------------------
   -- Log_Request_List --
   ----------------------

   use type Alog.Log_Request.Instance;

   package List_Of_Log_Requests_Package is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type => Log_Request.Instance);

   package LOLRP renames List_Of_Log_Requests_Package;

   type Log_Request_List is new LOLRP.List with null record;
   --  Log requests list. This doubly-linked list holds log request
   --  objects.

   -------------------
   -- Exception_Map --
   -------------------

   use type Ada.Exceptions.Exception_Occurrence_Access;

   function "<" (Left, Right : Ada.Task_Identification.Task_Id) return Boolean;
   --  Smaller-than function for Task_Id. Needed to use Task_Id as Key_Type for
   --  Ordered_Map.

   package Map_Of_Exception_Occurrences_Package is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Ada.Task_Identification.Task_Id,
        Element_Type => Ada.Exceptions.Exception_Occurrence_Access);

   package MOEOP renames Map_Of_Exception_Occurrences_Package;

   type Exception_Map is new MOEOP.Map with null record;
   --  Per-task Exception_Occurrence storage. This map works like a message box
   --  for exception occurrences handles which are stored on a per-caller
   --  (Task_Id) basis. Care must be taken with regards to memory management
   --  since the handles point to Exception_Occurrences on the heap. The memory
   --  of an occurrence pointed to by an access must be freed by the user.

end Alog.Containers;
