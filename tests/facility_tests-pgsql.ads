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

package Facility_Tests.PGSQL is

   type F_Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out F_Test);
   --  Initialize Test suite.

   procedure Set_Host_Name;
   --  Test set/get Host_Name.

   procedure Set_Host_Address;
   --  Test set Host_Address.

   procedure Set_Host_Port;
   --  Test set/get Host_Port.

   procedure Set_DB_Name;
   --  Test set/get DB_Name.

   procedure Set_Table_Name;
   --  Test set/get Table_Name.

   procedure Set_Level_Column_Name;
   --  Test set/get Level_Column_Name.

   procedure Set_Timestamp_Column_Name;
   --  Test set/get Timestamp_Column_Name.

   procedure Set_Message_Column_Name;
   --  Test set/get Message_Column_Name.

   procedure Set_Credentials;
   --  Test set/get Credentials.

   procedure Enable_SQL_Trace;
   --  Test the sql trace enable/disable functionality.

   procedure Disable_Write_Timestamp;
   --  Test the timestamp enable/disable functionality.

   procedure Disable_Write_Loglevel;
   --  Test the loglevel enable/disable functionality.

   procedure Write_Message;
   --  Test message writing.

end Facility_Tests.PGSQL;
