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
with Ada.Finalization;
with Ada.Task_Identification;

with Alog.Containers;

--  Alog Controlled Containers. This package holds controlled variants of
--  different Alog containers.
package Alog.Controlled_Containers is

   -----------------------------
   -- Controlled_Exception_Map --
   -----------------------------

   type Exception_Map is
     new Ada.Finalization.Limited_Controlled with private;
   --  Controlled variant of the exception map. To make memory management more
   --  robust only copies of Excpetion_Occurrences and not handles are returned
   --  by the map. The memory of an occurrence pointed to by a previously
   --  inserted handle is freed upon calling Delete, Clear or during
   --  finalization of the controlled type. Thus control over object inserted
   --  into this map resides with the controlled map.

   overriding
   procedure Finalize (Map : in out Exception_Map);
   --  Clean up any remaining elements in the map.

   procedure Insert
     (Map  : in out Exception_Map;
      Key  :        Ada.Task_Identification.Task_Id;
      Item :        Ada.Exceptions.Exception_Occurrence_Access);
   --  Insert the given Exception_Occurrence 'Element' with key 'Key' into
   --  the map.

   procedure Get
     (Map     : in out Exception_Map;
      Key     :        Ada.Task_Identification.Task_Id;
      Element :    out Ada.Exceptions.Exception_Occurrence);
   --  Get the Exception_Occurrence with key 'Key' from the map.

   procedure Delete
     (Map : in out Exception_Map;
      Key :        Ada.Task_Identification.Task_Id);
   --  Delete the Exception_Occurrence with key 'Key' from the map. Memory
   --  of the exception occurrence is freed.

   function Contains
     (Map : Exception_Map;
      Key : Ada.Task_Identification.Task_Id)
      return Boolean;
   --  Returns True if an element with key 'Key' is in the map.

   function Is_Empty (Map : Exception_Map) return Boolean;
   --  Returns True if the map is empty.

   procedure Clear (Map : in out Exception_Map);
   --  Remove all Exception_Occurrences in the map. Memory of the exception
   --  occurrences is freed.

private

   type Exception_Map is
     new Ada.Finalization.Limited_Controlled with record
      Data : Containers.Exception_Map;
   end record;

end Alog.Controlled_Containers;
