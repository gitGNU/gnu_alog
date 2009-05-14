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

with Alog.Facilities.File_Descriptor;

package body Facility_Tests is

   use Alog;
   use Alog.Facilities;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      Set_Name (T, "Tests for Alog Facilites");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Name'Access, "set facility name");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Threshold'Access, "set threshold");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Set_Name is
      F        : File_Descriptor.Instance;
      Expected : constant String := "TEST";
   begin
      F.Set_Name (Name => Expected);
      Assert (Condition => F.Get_Name = Expected,
              Message => "name not equal");
   end Set_Name;

   -------------------------------------------------------------------------

   procedure Set_Threshold is
      F        : File_Descriptor.Instance;
      Expected : constant Log_Level := DEBU;
   begin
      F.Set_Threshold (Level => Expected);
      Assert (Condition => F.Get_Threshold = Expected,
              Message => "Log_Level not equal");
   end Set_Threshold;

end Facility_Tests;
