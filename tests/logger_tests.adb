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
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  Ahven
with Ahven; use Ahven;

--  Alog
with Alog; use Alog;
with Alog.Logger;
with Alog.Helpers;
with Alog.Facilities;
with Alog.Facilities.File_Descriptor;
with Alog.Facilities.Syslog;
with Alog.Transforms;
with Alog.Transforms.Casing;

package body Logger_Tests is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out L_Test) is
   begin
      Set_Name (T, "Tests for Alog Logger");
      Ahven.Framework.Add_Test_Routine
        (T, Attach_Facility'Access,
         "attach a facility");
      Ahven.Framework.Add_Test_Routine
        (T, Detach_Facility_Instance'Access,
         "detach facility:instance");
      Ahven.Framework.Add_Test_Routine
        (T, Detach_Facility_Unattached'Access,
         "detach not attached facility");
      Ahven.Framework.Add_Test_Routine
        (T, Attach_Transform'Access,
         "attach a transform");
      Ahven.Framework.Add_Test_Routine
        (T, Detach_Transform_Instance'Access,
         "detach transform:instance");
      Ahven.Framework.Add_Test_Routine
        (T, Detach_Transform_Unattached'Access,
         "detach not attached transform");
      Ahven.Framework.Add_Test_Routine
        (T, Clear_A_Logger'Access,
         "clear logger");
      Ahven.Framework.Add_Test_Routine
        (T, Log_One_FD_Facility'Access,
         "log to one fd facility");
      Ahven.Framework.Add_Test_Routine
        (T, Log_Multiple_FD_Facilities'Access,
         "log to multiple fd facilities");
      Ahven.Framework.Add_Test_Routine
        (T, Log_FD_Facility_with_Transform'Access,
         "log to fd facility with lowercase transform");

   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (T : in out L_Test) is

      use Ahven.Framework;
      use Alog.Facilities;

      --  Files to clean after tests.
      subtype Count is Natural range 1 .. 4;

      Files : array (Count) of BS_Path.Bounded_String :=
        (BS_Path.To_Bounded_String ("./data/Log_One_FD_Facility"),
         BS_Path.To_Bounded_String ("./data/Log_Multiple_FD_Facilities1"),
         BS_Path.To_Bounded_String ("./data/Log_Multiple_FD_Facilities2"),
         BS_Path.To_Bounded_String ("./data/Log_FD_Facility_Lowercase")
        );
      F     : File_Type;
   begin
      for c in Count loop
         Open (File => F,
               Mode => In_File,
               Name => BS_Path.To_String (Files (c)));
         Delete (File => F);
      end loop;

      Finalize (Test_Case (T));

   exception
      when Error : Ada.IO_Exceptions.Name_Error =>
         null;
         --  File did not exist. Carry on.
      when Event : others =>
         Put_Line ("error occured while cleaning up: ");
         Put_Line (Ada.Exceptions.Exception_Name (Event));
         Put_Line (Ada.Exceptions.Exception_Message (Event));
   end Finalize;

   ---------------------
   -- Attach_Facility --
   ---------------------

   procedure Attach_Facility is
      Log      : Logger.Instance;
      Facility : Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
   begin
      Log.Attach_Facility (Facility => Facility);
      Assert (Condition => Log.Facility_Count = 1,
              Message => "could not attach facility");
   end Attach_Facility;

   ------------------------------
   -- Detach_Facility_Instance --
   ------------------------------

   procedure Detach_Facility_Instance is
      Log      : Logger.Instance;
      Facility : Facilities.Handle :=
        new Facilities.Syslog.Instance;
   begin
      Facility.Set_Name ("Syslog_Facility");
      Log.Attach_Facility (Facility => Facility);
      Assert (Condition => Log.Facility_Count = 1,
              Message   => "could not attach");
      Log.Detach_Facility (Facility => Facility);
      Assert (Condition => Log.Facility_Count = 0,
              Message   => "could not detach");
   end Detach_Facility_Instance;

   --------------------------------
   -- Detach_Facility_Unattached --
   --------------------------------

   procedure Detach_Facility_Unattached is
      Log      : Logger.Instance;
      Facility : Facilities.Handle :=
        new Facilities.Syslog.Instance;
   begin
      Facility.Set_Name ("Syslog_Facility");
      Log.Detach_Facility (Facility => Facility);
      Fail (Message => "not yet implemented");
   exception
      when Logger.Facility_Not_Found =>
         --  Free not attached facility, this is not done
         --  by the logger (since it was never attached).
         Alog.Logger.Free (Facility);
         --  Test passed.
   end Detach_Facility_Unattached;

   ----------------------
   -- Attach_Transform --
   ----------------------

   procedure Attach_Transform is
      Log      : Logger.Instance;
      Transform : Transforms.Handle :=
                    new Transforms.Casing.Instance;
   begin
      Log.Attach_Transform (Transform => Transform);
      Assert (Condition => Log.Transform_Count = 1,
              Message => "could not attach transform");
   end Attach_Transform;

   -------------------------------
   -- Detach_Transform_Instance --
   -------------------------------

   procedure Detach_Transform_Instance is
      Log      : Logger.Instance;
      Transform : Transforms.Handle :=
                    new Transforms.Casing.Instance;
   begin
      Transform.Set_Name ("Casing_Transform");
      Log.Attach_Transform (Transform => Transform);
      Assert (Condition => Log.Transform_Count = 1,
              Message   => "could not attach");
      Log.Detach_Transform (Transform => Transform);
      Assert (Condition => Log.Transform_Count = 0,
              Message   => "could not detach");
   end Detach_Transform_Instance;

   ---------------------------------
   -- Detach_Transform_Unattached --
   ---------------------------------

   procedure Detach_Transform_Unattached is
      Log      : Logger.Instance;
      Transform : Transforms.Handle :=
                    new Transforms.Casing.Instance;
   begin
      Transform.Set_Name ("Casing_Transform");
      Log.Detach_Transform (Transform => Transform);
      Fail (Message => "could detach unattached transform");
   exception
      when Logger.Transform_Not_Found =>
         --  Free not attached Transform, this is not done
         --  by the logger (since it was never attached).
         Alog.Logger.Free (Transform);
         --  Test passed.
   end Detach_Transform_Unattached;

   --------------------
   -- Clear_A_Logger --
   --------------------

   procedure Clear_A_Logger is
      Log       : Logger.Instance;
      Facility  : Facilities.Handle :=
                    new Facilities.File_Descriptor.Instance;
      Transform : Transforms.Handle :=
                    new Transforms.Casing.Instance;
   begin
      Log.Attach_Facility (Facility => Facility);
      Assert (Condition => Log.Facility_Count = 1,
              Message   => "could not attach facility");

      Log.Attach_Transform (Transform => Transform);
      Assert (Condition => Log.Transform_Count = 1,
              Message   => "could not attach transform");

      --  Clear it.
      Log.Clear;
      Assert (Condition => Log.Facility_Count = 0,
              Message   => "facility count is not 0");
      Assert (Condition => Log.Transform_Count = 0,
              Message   => "transform count is not 0");
   end Clear_A_Logger;

   -------------------------
   -- Log_One_FD_Facility --
   -------------------------

   procedure Log_One_FD_Facility is
      Log      : Logger.Instance;
      Facility : Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Testfile : String := "./data/Log_One_FD_Facility";
      Reffile  : String := "./data/Log_One_FD_Facility.ref";
   begin
      --  Call facility fd specific procedures.
      Facilities.File_Descriptor.Handle
        (Facility).Toggle_Write_Timestamp (Set => False);
      Facilities.File_Descriptor.Handle
        (Facility).Set_Logfile (Testfile);

      Log.Attach_Facility (Facility => Facility);
      Log.Log_Message (Level => DEBU,
                       Msg   => "Logger testmessage, one fd facility");

      --  Cleanup
      Log.Clear;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "files not equal");
   end Log_One_FD_Facility;

   --------------------------------
   -- Log_Multiple_FD_Facilities --
   --------------------------------

   procedure Log_Multiple_FD_Facilities is
      Log       : Logger.Instance;

      Facility1 : Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Testfile1 : String := "./data/Log_Multiple_FD_Facilities1";
      Reffile1  : String := "./data/Log_Multiple_FD_Facilities1.ref";

      Facility2 : Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Testfile2 : String := "./data/Log_Multiple_FD_Facilities2";
      Reffile2  : String := "./data/Log_Multiple_FD_Facilities2.ref";
   begin
      --  Set unique names.
      Facility1.Set_Name (Name => "Facility1");
      Facility2.Set_Name (Name => "Facility2");

      --  Call facility fd specific procedures.
      Facilities.File_Descriptor.Handle
        (Facility1).Toggle_Write_Timestamp (Set => False);
      Facilities.File_Descriptor.Handle
        (Facility1).Set_Logfile (Testfile1);

      Facilities.File_Descriptor.Handle
        (Facility2).Toggle_Write_Timestamp (Set => False);
      Facilities.File_Descriptor.Handle
        (Facility2).Set_Logfile (Testfile2);

      --  Set INFO-threshold for second facility.
      Facility2.Set_Threshold (Level => INFO);

      --  Attach both facilities to logger instance.
      Log.Attach_Facility (Facility => Facility1);
      Log.Attach_Facility (Facility => Facility2);

      --  Log two messages with different loglevels.
      Log.Log_Message (Level => DEBU,
                       Msg   => "Logger testmessage, multiple facilities");
      Log.Log_Message (Level => INFO,
                       Msg   => "Logger testmessage, multiple facilities");

      --  Cleanup
      Log.Clear;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile1,
               Filename2 => Testfile1),
              Message   => "file1 not equal");

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile2,
               Filename2 => Testfile2),
              Message   => "file2 not equal");
   end Log_Multiple_FD_Facilities;

   ------------------------------------
   -- Log_FD_Facility_with_Transform --
   ------------------------------------

   procedure Log_FD_Facility_with_Transform is
      Log       : Logger.Instance;
      Facility  : Facilities.Handle :=
                    new Facilities.File_Descriptor.Instance;
      Transform : Transforms.Handle :=
                    new Transforms.Casing.Instance;
      Testfile  : String := "./data/Log_FD_Facility_Lowercase";
      Reffile   : String := "./data/Log_FD_Facility_Lowercase.ref";
   begin
      --  Call facility fd specific procedures.
      Facilities.File_Descriptor.Handle
        (Facility).Toggle_Write_Timestamp (Set => False);
      Facilities.File_Descriptor.Handle
        (Facility).Set_Logfile (Testfile);

      --  Call casing transform specific procedures.
      Transforms.Casing.Handle
        (Transform).Set_Name ("lowercase");

      Log.Attach_Facility (Facility => Facility);
      Log.Attach_Transform (Transform => Transform);

      Facility.Add_Transform (Transform);

      Log.Log_Message (Level => DEBU,
                       Msg   => "Logger Test Message, " &
                       "FD Facility With Lowercase Transform");

      --  Cleanup
      Log.Clear;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "files not equal");
   end Log_FD_Facility_with_Transform;

end Logger_Tests;
