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

--  Tests for Alog facility components.
package Facility_Tests is

   type FTest is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out FTest);
   --  Initialize Test suite.

   procedure Finalize (T : in out FTest);
   --  Teardown Test suite.


   --  GENERIC


   procedure Set_Name;
   --  Set name of Facility test.

   procedure Set_Threshold;
   --  Test set threshold.


   --  FD Facility


   procedure Set_Valid_Logfile_Fd;
   --  Set correct Fd test.

   procedure Set_Invalid_Logfile_Fd;
   --  Set illegal Fd test.

   procedure Write_Message_Fd;
   --  Test message writing.

   procedure Teardown_Fd;
   --  Test Facility cleanup;

   procedure Disable_Write_Timestamp_Fd;
   --  Test the timestamp enable/disable functionality.

   procedure Disable_Write_Loglevel_Fd;
   --  Test the loglevel enable/disable functionality.

   procedure Set_Threshold_Fd;
   --  Test Threshold-functionality with fd facility.

   --  Syslog Facility

   procedure Init_Syslog;
   --  Test initialization of syslog-facility.

end Facility_Tests;
