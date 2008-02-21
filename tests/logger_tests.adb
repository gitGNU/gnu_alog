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
with Alog.Logger;
with Alog.Facilities.File_Descriptor;

package body Logger_Tests is

   procedure Initialize (T : in out LTest) is
   begin
      Set_Name (T, "Tests for Alog Logger");
      Ahven.Framework.Add_Test_Routine
        (T, Attach_A_Facility'Access, "attach a facility");
      Ahven.Framework.Add_Test_Routine
        (T, Detach_A_Facility'Access, "detach a facility");
   end Initialize;

   ---------------------
   -- Attach_Facility --
   ---------------------

   procedure Attach_A_Facility is
      Logger   : Alog.Logger.Instance;
      Facility : Alog.Facilities.Handle := new
        Alog.Facilities.File_Descriptor.Instance;
   begin
      Logger.Attach_Facility (F => Facility);
      Assert (Condition => Logger.Facility_Count = 1,
              Message => "Could not attach facility");
   end Attach_A_Facility;

   ---------------------
   -- Detach_Facility --
   ---------------------

   procedure Detach_A_Facility is
   begin
      Fail ("Not yet implemented!");
   end Detach_A_Facility;

end Logger_Tests;
