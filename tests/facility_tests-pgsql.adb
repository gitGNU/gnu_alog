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

--  Ada
with Ada.Exceptions;

--  Ahven
with Ahven; use Ahven;

--  Alog
with Alog;  use Alog;
with Alog.Facilities; use Alog.Facilities;
with Alog.Facilities.Pgsql;

--  APQ
with APQ;

package body Facility_Tests.PGSQL is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out F_Test) is
   begin
      Set_Name (T, "Tests for Alog Facility PGSQL");
      Ahven.Framework.Add_Test_Routine
        (T, Write_Message'Access, "log a message to PGSQL database");
   end Initialize;

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message is
      F : Alog.Facilities.Pgsql.Instance;
   begin
      F.Toggle_Write_Timestamp (Set => True);

      F.Set_DB_Name (DB_Name => "alog");
      F.Set_Credentials (Username => "alog",
                         Password => "foobar");

      --  Setup facility (open db connection, etc)
      F.Setup;

      F.Write_Message (Msg   => "Test message");

      --  Cleanup
      F.Teardown;
   exception
      when others =>
         Fail (Message => "could not write msg to database");
   end Write_Message;

end Facility_Tests.PGSQL;
