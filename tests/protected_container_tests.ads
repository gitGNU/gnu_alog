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

with Ahven.Framework;

--  Tests for Alog protected containers.
package Protected_Container_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Log_Request_List_Get_Put;
   --  Test log request list get and put operations.

   procedure Log_Request_List_Clear;
   --  Test log request list clear operation.

   procedure Log_Request_List_Done;
   --  Test log request list Done operation.

   procedure Exception_Map_Insert_Get;
   --  Test insert and get for protected exception map.

   procedure Exception_Map_Delete;
   --  Test delete for protected exception map.

   procedure Exception_Map_Clear;
   --  Test clear for protected exception map.

end Protected_Container_Tests;
