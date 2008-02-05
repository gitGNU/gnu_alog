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
with Logger_Tests;

procedure Runner is
   S : Ahven.Framework.Test_Suite_Access :=
     Ahven.Framework.Create_Suite ("All Alog tests");
   pragma Linker_Options ("-lahven");
begin
   Ahven.Framework.Add_Test (S.all, new Facility_Tests.FTest);
   Ahven.Framework.Add_Test (S.all, new Logger_Tests.LTest);

   Ahven.Text_Runner.Run (S);
   Ahven.Framework.Release_Suite (S);
end Runner;
