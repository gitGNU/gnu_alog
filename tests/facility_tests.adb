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

with Ada.Calendar.Time_Zones;

with Ahven; use Ahven;

with Alog.Facilities.File_Descriptor;
with Alog.Transforms.Casing;

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
      Ahven.Framework.Add_Test_Routine
        (T, Toggle_Loglevel'Access, "toggle loglevel");
      Ahven.Framework.Add_Test_Routine
        (T, Toggle_Timestamp'Access, "toggle timestamp");
      Ahven.Framework.Add_Test_Routine
        (T, Toggle_UTC_Timestamp'Access, "toggle UTC timestamp");
      Ahven.Framework.Add_Test_Routine
        (T, Transform_Handling'Access, "transform handling");
      Ahven.Framework.Add_Test_Routine
        (T, Timestamp_Creation'Access, "timestamp creation");
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

   -------------------------------------------------------------------------

   procedure Timestamp_Creation is
      use Ada.Calendar;
      use Ada.Calendar.Time_Zones;

      F : File_Descriptor.Instance;

      Ref_Time  : constant Time   := Time_Of
        (Year    => 2009,
         Month   => 10,
         Day     => 10,
         Seconds => 7255.0);
      Ref_Stamp : constant String := "Oct 10 2009 02:00:55";

      --  Adding the UTC time offset to the reference time should lead to the
      --  same timestamp string when UTC timestamps are enabled since UTC time
      --  is timezone-dependent time minus the UTC offset at that given time.

      Ref_UTC_Time : constant Time := Ref_Time + Duration
        (UTC_Time_Offset (Ref_Time)) * 60;
   begin
      Assert (Condition => Ref_Stamp = F.Get_Timestamp (Time => Ref_Time),
              Message   => "Timestamp mismatch");

      F.Toggle_UTC_Timestamp (State => True);
      Assert (Condition => F.Get_Timestamp (Time => Ref_UTC_Time) = Ref_Stamp,
              Message   => "UTC timestamp mismatch!");
   end Timestamp_Creation;

   -------------------------------------------------------------------------

   procedure Toggle_Loglevel is
      F : File_Descriptor.Instance;
   begin
      Assert (Condition => not F.Is_Write_Loglevel,
              Message   => "Loglevel writing is 'True' by default");

      F.Toggle_Write_Loglevel (State => True);
      Assert (Condition => F.Is_Write_Loglevel,
              Message   => "Loglevel writing not 'True'");
   end Toggle_Loglevel;

   -------------------------------------------------------------------------

   procedure Toggle_Timestamp is
      F : File_Descriptor.Instance;
   begin
      Assert (Condition => F.Is_Write_Timestamp,
              Message   => "Timestamp writing is 'False' by default");
      F.Toggle_Write_Timestamp (State => False);
      Assert (Condition => not F.Is_Write_Timestamp,
              Message   => "Timestamp writing not 'False'");
   end Toggle_Timestamp;

   -------------------------------------------------------------------------

   procedure Toggle_UTC_Timestamp is
      F : File_Descriptor.Instance;
   begin
      Assert (Condition => not F.Is_UTC_Timestamp,
              Message   => "Default should be 'False'");

      F.Toggle_UTC_Timestamp (State => True);
      Assert (Condition => F.Is_UTC_Timestamp,
              Message   => "Expected 'True'");
   end Toggle_UTC_Timestamp;

   -------------------------------------------------------------------------

   procedure Transform_Handling is
      T_Name    : constant String            := "Test_Transform";
      F         : File_Descriptor.Instance;
      Transform : constant Transforms.Handle :=
        new Transforms.Casing.Instance;

      procedure Check_Transform (Transform_Handle : in out Transforms.Handle);
      --  Verify that transformy with given name is present in the logger.

      procedure Check_Transform (Transform_Handle : in out Transforms.Handle)
      is
         use type Transforms.Handle;
      begin
         Assert (Condition => Transform_Handle = Transform,
                 Message   => "transform mismatch");
      end Check_Transform;

   begin
      Assert (Condition => F.Transform_Count = 0,
              Message   => "Transform count not 0");

      Transform.Set_Name (Name => T_Name);
      F.Add_Transform (Transform => Transform);

      Assert (Condition => F.Transform_Count = 1,
              Message   => "Unable to add transform");

      begin
         F.Add_Transform (Transform => Transform);
         Fail (Message => "Added existing transform");

      exception
         when Transform_Already_Present =>
            null;
      end;

      F.Update (Name    => T_Name,
                Process => Check_Transform'Access);

      F.Remove_Transform (Name => T_Name);
      Assert (Condition => F.Transform_Count = 0,
              Message   => "Unable to remove transform");

      begin
         F.Remove_Transform (Name => T_Name);
         Fail (Message => "Removed nonexistent transform");

      exception
         when Facilities.Transform_Not_Found =>
            null;
      end;

      begin
         F.Update (Name    => T_Name,
                   Process => Check_Transform'Access);

      exception
         when Facilities.Transform_Not_Found =>
            null;
      end;
   end Transform_Handling;

end Facility_Tests;
