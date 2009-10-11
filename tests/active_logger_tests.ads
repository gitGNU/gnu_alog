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

--  Tests for Alog active logger component.
package Active_Logger_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize Test suite.

   procedure Finalize (T : in out Testcase);
   --  Teardown Test suite.

   procedure Attach_Facility;
   --  Test Facility attaching.

   procedure Update_Facility;
   --  Test Facility update operation.

   procedure Detach_Facility_Instance;
   --  Test Facility detaching by name.

   procedure Detach_Facility_Unattached;
   --  Test Facility detaching of active logger with a un-attached instance.
   --  Verify that logger remains responsive after exception.

   procedure Attach_Transform;
   --  Test Transform attaching.

   procedure Detach_Transform_Instance;
   --  Test Transform detaching by name.

   procedure Detach_Transform_Unattached;
   --  Test Transform detaching of tasked logger with un-attached instance.
   --  Verify  that active logger remains responsive after exception.

   procedure Clear_A_Logger;
   --  Test Logger cleanup.

   procedure Log_Multiple_FD_Facilities;
   --  Test logging to multiple fd based facilities.

   procedure Log_FD_Facility_with_Transform;
   --  Test logging to fd based facility with lowercase transform.

   procedure Verify_Logger_Initialization;
   --  Test active logger instance initialization behavior.

   procedure Verify_Logger_Exception_Handling;
   --  Test active logger instance exception handling.

   procedure Default_Facility_Handling;
   --  Test attaching/detaching of default facility.

   procedure Tasked_One_FD_Facility;
   --  Test tasked logging to active logger with one fd based facility.

   procedure Verify_Iterate_Facilities;
   --  Test iterate for facilities of active logger.

end Active_Logger_Tests;
