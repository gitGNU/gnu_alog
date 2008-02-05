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

with Ahven; use Ahven;

package body Logger_Tests is

   procedure Initialize (T : in out LTest) is
   begin
      Set_Name (T, "Tests for Alog Logger");
      Ahven.Framework.Add_Test_Routine
        (T, Attach_Facility'Access, "Attach a Facility");
      Ahven.Framework.Add_Test_Routine
        (T, Detach_Facility'Access, "Detach a Facility");
   end Initialize;

   ---------------------
   -- Attach_Facility --
   ---------------------

   procedure Attach_Facility is
   begin
      Fail ("Not yet implemented!");
   end Attach_Facility;

   ---------------------
   -- Detach_Facility --
   ---------------------

   procedure Detach_Facility is
   begin
      Fail ("Not yet implemented!");
   end Detach_Facility;

end Logger_Tests;
