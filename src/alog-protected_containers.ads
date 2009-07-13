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

with Alog.Log_Request;
with Alog.Containers.Controlled_Map;

--  Alog Protected Containers. This package holds protected variants of
--  different Alog containers which are safe for concurrent access.
package Alog.Protected_Containers is

   ----------------------
   -- Log_Request_List --
   ----------------------

   protected type Log_Request_List is

      procedure Put (Element : Log_Request.Instance);
      --  Put an element at the end of the request list.

      entry Get (Element : out Log_Request.Instance);
      --  Get the first element from the list (and delete it).

      procedure Done;
      --  Signal successfull processing of request previously gotten from list.

      entry All_Done;
      --  This procedure blocks until the list is empty and there are no pending
      --  requests. A requests is pending when it is taken off the list via Get
      --  but it's successfull processing has not been signaled back via the
      --  procedure Done.

      procedure Clear;
      --  Clear the request list by deleting all log requests.

      function Length return Natural;
      --  Return the number of elements in the list.

      function Pending return Natural;
      --  Return the number of pending requests.

   private

      Requests           : Containers.Log_Request_List;
      Requests_Available : Boolean := False;
      Pending_Counter    : Natural := 0;

   end Log_Request_List;
   --  Protected variant of the log request list. This list holds log request
   --  objects and is safe for concurrent access. It operates in FIFO-Mode.

   -----------------------------
   -- Protected_Exception_Map --
   -----------------------------

   type Exception_Storage is limited private;

   protected type Protected_Exception_Map is

      procedure Insert
        (Key  : Ada.Task_Identification.Task_Id;
         Item : Ada.Exceptions.Exception_Occurrence_Access);
      --  Insert the given Exception_Occurrence 'Element' with key 'Key' into
      --  the map.

      entry Get
        (Key     :     Ada.Task_Identification.Task_Id;
         Element : out Ada.Exceptions.Exception_Occurrence);
      --  Get the Exception_Occurrence with key 'Key' from the map.

      procedure Delete (Key : Ada.Task_Identification.Task_Id);
      --  Delete the Exception_Occurrence with key 'Key' from the map. Memory
      --  of the exception occurrence is freed. The user must make sure to not
      --  access deleted elements.

      function Contains (Key : Ada.Task_Identification.Task_Id) return Boolean;
      --  Returns True if an element with key 'Key' is in the map.

      procedure Clear;
      --  Remove all Exception_Occurrences in the map. Memory of the exception
      --  occurrences is freed.

   private

      Data                 : Exception_Storage;
      Exceptions_Available : Boolean := False;

   end Protected_Exception_Map;
   --  Protected variant of the exception map. To make memory management more
   --  robust only copies of Excpetion_Occurrences and not handles are returned
   --  by the map. The memory of an occurrence pointed to by a previously
   --  inserted handle is freed upon calling Delete, Clear or during
   --  finalization of the protected type

private

   function "<" (Left, Right : Ada.Task_Identification.Task_Id) return Boolean;
   --  Smaller-than function for Task_Id. Needed to use Task_Id as Key_Type.

   package Map_Of_Exceptions_Package is new Alog.Containers.Controlled_Map
     (Key_Type       => Ada.Task_Identification.Task_Id,
      Element_Type   => Ada.Exceptions.Exception_Occurrence,
      Element_Handle => Ada.Exceptions.Exception_Occurrence_Access);

   package MOEP renames Map_Of_Exceptions_Package;

   type Exception_Storage is limited new MOEP.Map with null record;

end Alog.Protected_Containers;
