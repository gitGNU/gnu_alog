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
        (T, Transform_Handling'Access, "transform handling");
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

   procedure Transform_Handling is
      T_Name : constant String := "Test_Transform";
      F      : File_Descriptor.Instance;
      T      : aliased Transforms.Casing.Instance;
   begin
      Assert (Condition => F.Transform_Count = 0,
              Message   => "Transform count not 0");

      T.Set_Name (Name => T_Name);
      F.Add_Transform (Transform => T'Unchecked_Access);

      Assert (Condition => F.Transform_Count = 1,
              Message   => "Unable to add transform");

      begin
         F.Add_Transform (Transform => T'Unchecked_Access);
         Fail (Message => "Added existing transform");

      exception
         when Transform_Already_Present =>
            null;
      end;

      declare
         use type Transforms.Handle;

         T_Handle : Transforms.Handle;
      begin
         T_Handle := F.Get_Transform (Name => T_Name);

         Assert (Condition => T_Handle = T'Unchecked_Access,
                 Message   => "Transform mismatch");
      end;

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

      declare
         T_Handle : Transforms.Handle;
         pragma Unreferenced (T_Handle);
      begin
         T_Handle := F.Get_Transform (Name => T_Name);
         Fail (Message => "Got nonexistent transform");

      exception
         when Facilities.Transform_Not_Found =>
            null;
      end;
   end Transform_Handling;

end Facility_Tests;
