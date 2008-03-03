--
--  Copyright (c) 2008,
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

--  Tests for Alog logger component.
package Logger_Tests is

   type L_Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out L_Test);
   --  Initialize Test suite.

   procedure Finalize (T : in out L_Test);
   --  Teardown Test suite.

   procedure Attach_A_Facility;
   --  Test Facility attaching.

   procedure Detach_A_Facility;
   --  Test Facility detaching.

   procedure Clear_A_Logger;
   --  Test Logger cleanup.

   procedure Log_One_FD_Facility;
   --  Test logging to one fd based facility.

   procedure Log_Multiple_FD_Facilities;
   --  Test logging to multiple fd based facilities.

end Logger_Tests;
