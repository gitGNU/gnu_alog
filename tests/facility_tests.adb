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

--  Ahven
with Ahven; use Ahven;

--  Alog
with Alog; use Alog;
with Alog.Facilities; use Alog.Facilities;
with Alog.Facilities.File_Descriptor;
with Alog.Facilities.Syslog;

package body Facility_Tests is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out F_Test) is
   begin
      Set_Name (T, "Tests for Alog Facilites");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Name'Access, "set facility name");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Threshold'Access, "set threshold");
      Ahven.Framework.Add_Test_Routine
        (T, Init_Syslog'Access, "init syslog facility");
   end Initialize;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name is
      F        : File_Descriptor.Instance;
      Expected : String := "TEST";
   begin
      F.Set_Name (Name => Expected);
      Assert (Condition => F.Get_Name = Expected,
              Message => "name not equal");
   end Set_Name;

   -------------------
   -- Set_Threshold --
   -------------------

   procedure Set_Threshold is
      F        : File_Descriptor.Instance;
      Expected : Log_Level := DEBU;
   begin
      F.Set_Threshold (Level => Expected);
      Assert (Condition => F.Get_Threshold = Expected,
              Message => "Log_Level not equal");
   end Set_Threshold;

   -----------------
   -- Init_Syslog --
   -----------------

   procedure Init_Syslog is
      F : Syslog.Instance;
   begin
      Fail (Message => "not yet implemented");
   end Init_Syslog;

end Facility_Tests;
