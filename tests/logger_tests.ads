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

   procedure Attach_Facility;
   --  Test Facility attaching.

   procedure Detach_Facility_Instance;
   --  Test Facility detaching by instance.

   procedure Detach_Facility_Unattached;
   --  Test Facility detaching with a not attached instance
   --  (should fail).

   procedure Attach_Transform;
   --  Test Transform attaching.

   procedure Detach_Transform_Instance;
   --  Test Transform detaching by instance.

   procedure Detach_Transform_Unattached;
   --  Test Transform detaching with un-attached instance

   procedure Clear_A_Logger;
   --  Test Logger cleanup.

   procedure Log_One_FD_Facility;
   --  Test logging to one fd based facility.

   procedure Log_Multiple_FD_Facilities;
   --  Test logging to multiple fd based facilities.

   procedure Log_FD_Facility_with_Transform;
   --  Test logging to fd based facility with lowercase transform.

   procedure Log_One_Tasked_FD_Facility;
   --  Test tasked logging to one fd based facility.

end Logger_Tests;
