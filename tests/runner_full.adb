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

with Ahven.Text_Runner;
with Ahven.Framework;
with Facility_Tests;
with Facility_Tests.FD;
with Facility_Tests.SMTP;
with Facility_Tests.XMPP;
with Facility_Tests.PGSQL;
with Transform_Tests;
with Transform_Tests.Casing;
with Logger_Tests;

procedure Runner_Full is
   S : Ahven.Framework.Test_Suite_Access :=
     Ahven.Framework.Create_Suite ("Alog full tests");
   pragma Linker_Options ("-lahven");
   pragma Linker_Options ("-laws");
   pragma Linker_Options ("-lapq");
begin
   --  Facility tests
   Ahven.Framework.Add_Test (S.all, new Facility_Tests.F_Test);
   Ahven.Framework.Add_Test (S.all, new Facility_Tests.FD.F_Test);
   Ahven.Framework.Add_Test (S.all, new Facility_Tests.SMTP.F_Test);
   Ahven.Framework.Add_Test (S.all, new Facility_Tests.XMPP.F_Test);
   Ahven.Framework.Add_Test (S.all, new Facility_Tests.PGSQL.F_Test);
   Ahven.Framework.Add_Test (S.all, new Transform_Tests.T_Test);
   Ahven.Framework.Add_Test (S.all, new Transform_Tests.Casing.T_Test);
   Ahven.Framework.Add_Test (S.all, new Logger_Tests.L_Test);

   Ahven.Text_Runner.Run (S);
   Ahven.Framework.Release_Suite (S);
end Runner_Full;
