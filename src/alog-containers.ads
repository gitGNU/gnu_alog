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

end Alog.Containers;
