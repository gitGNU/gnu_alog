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

with Ada.Directories;
with Ada.Exceptions.Is_Null_Occurrence;
with Ada.Task_Identification;

with Ahven;

with Alog.Helpers;
with Alog.Logger;
with Alog.Tasked_Logger;
with Alog.Facilities.File_Descriptor;
with Alog.Facilities.Syslog;
with Alog.Facilities.Mock;
with Alog.Transforms.Casing;

package body Tasked_Logger_Tests is

   use Ahven;
   use Alog;

   Counter : Natural := 0;
   --  Iterate counter.

   procedure Do_Nothing (Facility_Handle : Facilities.Handle) is null;
   --  Just do nothing.

   procedure Inc_Counter (F_Handle : Facilities.Handle);
   --  Increment iterate counter.

   procedure Raise_Exception (F_Handle : Facilities.Handle);
   --  Raise constraint error.

   procedure Enable_Facility_Timestamp (Facility_Handle : Facilities.Handle);

   -------------------------------------------------------------------------

   procedure Attach_Transform is
      Log       : Tasked_Logger.Instance (Init => False);
      Transform : constant Transforms.Handle := new Transforms.Casing.Instance;
      Count     : Natural := Natural'Last;
   begin
      Log.Transform_Count (Count => Count);

      Assert (Condition => Count = 0,
              Message   => "transform count not 0");

      Log.Attach_Transform (Transform => Transform);
      Log.Transform_Count (Count => Count);
      Assert (Condition => Count = 1,
              Message => "could not attach transform");

      begin
         Log.Attach_Transform (Transform => Transform);
         Fail (Message => "attached duplicate transform");

      exception
         when Logger.Transform_Already_Present =>
            null;
      end;
   end Attach_Transform;

   -------------------------------------------------------------------------

   procedure Default_Facility_Handling is
      Logger1 : Tasked_Logger.Instance (Init => False);
      Logger2 : Tasked_Logger.Instance (Init => True);
      F_Count : Natural := Natural'Last;
   begin
      Logger1.Attach_Default_Facility;
      Logger1.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 1,
              Message   => "Unable to attach facility");

      Logger1.Attach_Default_Facility;
      Logger1.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 1,
              Message   => "Attached facility twice");

      Logger1.Detach_Default_Facility;
      Logger1.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 0,
              Message   => "Unable to detach facility");

      Logger2.Attach_Default_Facility;
      Logger2.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 1,
              Message   => "Attached facility to initialzed logger");

      Logger2.Detach_Default_Facility;
      Logger2.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 0,
              Message   => "Unable to detach facility from initialized logger");
   end Default_Facility_Handling;

   -------------------------------------------------------------------------

   procedure Detach_Facility_Unattached is
      Log      : Tasked_Logger.Instance (Init => False);
      Facility : Facilities.Handle :=
        new Facilities.Syslog.Instance;
   begin
      begin
         Facility.Set_Name ("Syslog_Facility");
         Log.Detach_Facility (Name => Facility.Get_Name);
         Fail (Message => "could detach unattached facility");

      exception
         when Logger.Facility_Not_Found =>
            --  Free not attached facility, this is not done by the logger
            --  (since it was never attached).
            Alog.Logger.Free (Facility);
      end;

      declare
         F_Count : Natural := Natural'Last;
      begin

         --  Tasking_Error will be raised if tasked logger has terminated due to
         --  an unhandled exception.

         Log.Facility_Count (Count => F_Count);

      end;

   end Detach_Facility_Unattached;

   -------------------------------------------------------------------------

   procedure Detach_Transform is
      Log       : Tasked_Logger.Instance (Init => False);
      Transform : constant Transforms.Handle := new Transforms.Casing.Instance;
      Count     : Natural := 0;
   begin
      Transform.Set_Name ("Casing_Transform");
      Log.Attach_Transform (Transform => Transform);
      Log.Transform_Count (Count => Count);
      Assert (Condition => Count = 1,
              Message   => "Unable to attach transform");

      Log.Detach_Transform (Name => Transform.Get_Name);
      Log.Transform_Count (Count => Count);
      Assert (Condition => Count = 0,
              Message   => "Unable to detach transform");
   end Detach_Transform;

   -------------------------------------------------------------------------

   procedure Detach_Transform_Unattached is
      Log       : Tasked_Logger.Instance (Init => False);
      Transform : Transforms.Handle :=
        new Transforms.Casing.Instance;
   begin
      begin
         Transform.Set_Name ("Casing_Transform");
         Log.Detach_Transform (Name => Transform.Get_Name);
         Fail (Message => "could detach unattached transform");

      exception
         when Logger.Transform_Not_Found =>
            --  Free not attached Transform, this is not done by the logger
            --  (since it was never attached).
            Alog.Logger.Free (Transform);
      end;

      declare
         T_Count : Natural := Natural'Last;
      begin

         --  Tasking_Error will be raised if tasked logger has terminated due to
         --  an unhandled exception.

         Log.Transform_Count (Count => T_Count);

      end;

   end Detach_Transform_Unattached;

   -------------------------------------------------------------------------

   procedure Enable_Facility_Timestamp
     (Facility_Handle : Facilities.Handle)
   is
   begin
      Facility_Handle.Toggle_Write_Timestamp (State => True);
   end Enable_Facility_Timestamp;

   -------------------------------------------------------------------------

   procedure Finalize (T : in out Testcase) is

      use Ahven.Framework;
      use Alog.Facilities;

      Filename : constant String := "./data/Tasked_Log_One_FD_Facility";
   begin
      if Ada.Directories.Exists (Name => Filename) then
         Ada.Directories.Delete_File (Name => Filename);
      end if;

      Finalize (Test_Case (T));
   end Finalize;

   -------------------------------------------------------------------------

   procedure Inc_Counter (F_Handle : Facilities.Handle)
   is
      pragma Unreferenced (F_Handle);
   begin
      Counter := Counter + 1;
   end Inc_Counter;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      Set_Name (T, "Tests for Alog tasked Logger");
      Ahven.Framework.Add_Test_Routine
        (T, Update_Facility'Access,
         "update a facility");
      Ahven.Framework.Add_Test_Routine
        (T, Detach_Facility_Unattached'Access,
         "detach not attached facility");
      Ahven.Framework.Add_Test_Routine
        (T, Attach_Transform'Access,
         "attach a transform");
      Ahven.Framework.Add_Test_Routine
        (T, Detach_Transform_Unattached'Access,
         "detach not attached transform");
      Ahven.Framework.Add_Test_Routine
        (T, Log_One_FD_Facility'Access,
         "log with tasked logger");
      Ahven.Framework.Add_Test_Routine
        (T, Verify_Logger_Initialization'Access,
         "tasked logger initialization behavior");
      Ahven.Framework.Add_Test_Routine
        (T, Attach_Transform'Access,
         "tasked attach a transform");
      Ahven.Framework.Add_Test_Routine
        (T, Detach_Facility_Unattached'Access,
         "tasked detach not attached facility");
      Ahven.Framework.Add_Test_Routine
        (T, Detach_Transform'Access,
         "tasked detach transform");
      Ahven.Framework.Add_Test_Routine
        (T, Detach_Transform_Unattached'Access,
         "tasked detach not attached transform");
      Ahven.Framework.Add_Test_Routine
        (T, Logger_Exception_Handling'Access,
         "tasked logger exception handling");
      Ahven.Framework.Add_Test_Routine
        (T, Default_Facility_Handling'Access,
         "tasked default facility handling");
      Ahven.Framework.Add_Test_Routine
        (T, Iterate_Facilities'Access,
         "tasked iterate facilities");
      Ahven.Framework.Add_Test_Routine
        (T, Iterate_Facilities_Exceptions'Access,
         "tasked iterate facilities (exceptions)");
      Ahven.Framework.Add_Test_Routine
        (T, Loglevel_Handling'Access,
         "loglevel handling");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Iterate_Facilities is
      Log       : Tasked_Logger.Instance (Init => False);

      Facility1 : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Facility2 : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
   begin
      Counter := 0;

      Facility1.Set_Name (Name => "Facility1");
      Facility2.Set_Name (Name => "Facility2");

      Log.Attach_Facility (Facility => Facility1);
      Log.Attach_Facility (Facility => Facility2);

      Log.Iterate (Process => Inc_Counter'Access);

      Log.Clear;
      Assert (Condition => Counter = 2,
              Message   => "counter not 2");
   end Iterate_Facilities;

   -------------------------------------------------------------------------

   procedure Iterate_Facilities_Exceptions is
      use Ada.Exceptions;

      Log       : Tasked_Logger.Instance (Init => False);
      Facility1 : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      EO        : Exception_Occurrence;
   begin
      Facility1.Set_Name (Name => "Facility1");

      Log.Attach_Facility (Facility => Facility1);
      Log.Iterate (Process => Raise_Exception'Access);

      Log.Get_Last_Exception (Occurrence => EO);
      Assert
        (Condition => Exception_Name (X => EO) = "CONSTRAINT_ERROR",
         Message   => "Expected Constraint_Error");

      Log.Iterate (Process => Inc_Counter'Access);
      Log.Get_Last_Exception (Occurrence => EO);
      Assert
        (Condition => Is_Null_Occurrence (X => EO),
         Message   => "Exception not Null_Occurence");

      Log.Clear;
   end Iterate_Facilities_Exceptions;

   -------------------------------------------------------------------------

   procedure Log_One_FD_Facility is
      Log          : Tasked_Logger.Instance;
      F_Count      : Natural;
      Fd_Facility1 : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Fd_Facility2 : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;

      Testfile     : constant String := "./data/Tasked_Log_One_FD_Facility";
      Reffile      : constant String := "./data/Log_One_FD_Facility.ref";
   begin
      Fd_Facility1.Set_Name (Name => "Fd_Facility1");
      Fd_Facility1.Toggle_Write_Timestamp (State => False);

      Facilities.File_Descriptor.Handle
        (Fd_Facility1).Set_Logfile (Path => Testfile);

      Log.Attach_Facility (Facility => Fd_Facility1);
      Log.Attach_Facility (Facility => Fd_Facility2);

      Log.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 2,
              Message   => "facility count not 2");

      Log.Detach_Facility (Name => Fd_Facility2.Get_Name);
      Log.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 1,
              Message   => "facility count not 1");

      Log.Log_Message (Level => Debug,
                       Msg   => "Logger testmessage, one fd facility");

      Log.Clear;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "files not equal");
   end Log_One_FD_Facility;

   -------------------------------------------------------------------------

   procedure Logger_Exception_Handling is
      use Ada.Exceptions;

      Log           : Tasked_Logger.Instance;
      Mock_Facility : constant Facilities.Handle :=
        new Facilities.Mock.Instance;
      EO            : Exception_Occurrence;
   begin
      Log.Get_Last_Exception (Occurrence => EO);
      Assert
        (Condition => Is_Null_Occurrence (X => EO),
         Message   => "Exception not Null_Occurence");

      Log.Attach_Facility (Facility => Mock_Facility);
      Log.Log_Message (Level => Debug,
                       Msg   => "Test message");

      Log.Get_Last_Exception (Occurrence => EO);
      Assert
        (Condition => Exception_Name (X => EO) = "CONSTRAINT_ERROR",
         Message   => "Expected Constraint_Error");
      Assert
        (Condition => Exception_Message (X => EO) =
           Facilities.Mock.Exception_Message,
         Message   => "Found wrong exception message");

      --  Exception handling with explicit caller ID.

      Log.Log_Message (Level  => Debug,
                       Msg    => "Test message with caller ID",
                       Caller => Ada.Task_Identification.Current_Task);
      Log.Get_Last_Exception
        (Occurrence => EO,
         Caller     => Ada.Task_Identification.Current_Task);
      Assert
        (Condition => Exception_Name (X => EO) = "CONSTRAINT_ERROR",
         Message   => "Expected Constraint_Error for specific caller");
      Assert
        (Condition => Exception_Message (X => EO) =
           Facilities.Mock.Exception_Message,
         Message   => "Found wrong exception message for specific caller");

      Log.Detach_Facility (Name => Mock_Facility.Get_Name);
      Log.Log_Message (Level => Debug,
                       Msg   => "Test message 2");

      Log.Get_Last_Exception (Occurrence => EO);
      Assert
        (Condition => Is_Null_Occurrence (X => EO),
         Message   => "Exception not reset");
   end Logger_Exception_Handling;

   -------------------------------------------------------------------------

   procedure Loglevel_Handling is
      Log : Tasked_Logger.Instance;
   begin
      Log.Set_Loglevel (Level => Emergency);
      declare
         My_Level : Log_Level;
      begin
         Log.Get_Loglevel (Level => My_Level);
         Assert (Condition => My_Level = Emergency,
                 Message   => "Loglevel mismatch");
      end;

      Log.Set_Source_Loglevel (Source => "Foo",
                               Level  => Info);
      declare
         Source_Level : Log_Level;
      begin
         Log.Get_Source_Loglevel (Source => "Foo",
                                  Level  => Source_Level);
         Assert (Condition => Source_Level = Info,
                 Message   => "Source loglevel mismatch");
      end;

      declare
         Source_Level : Log_Level;
      begin
         Log.Get_Source_Loglevel (Source => "Bar",
                                  Level  => Source_Level);
         Fail (Message => "Expected No_Source_Loglevel");

      exception
         when Logger.No_Source_Loglevel =>
            null;
      end;
   end Loglevel_Handling;

   -------------------------------------------------------------------------

   procedure Raise_Exception (F_Handle : Facilities.Handle) is
   begin
      raise Constraint_Error with "DON'T PANIC! This is a test exception!";
   end Raise_Exception;

   -------------------------------------------------------------------------

   procedure Update_Facility is
      use Ada.Exceptions;

      Log : Tasked_Logger.Instance (Init => False);
      EO  : Ada.Exceptions.Exception_Occurrence;
   begin
      Log.Update (Name    => "Nonexistent",
                  Process => Do_Nothing'Access);

      Log.Get_Last_Exception (Occurrence => EO);
      Assert (Condition => Exception_Name (X => EO) =
                "ALOG.LOGGER.FACILITY_NOT_FOUND",
              Message   => "Expected Facility_Not_Found");

      declare
         Facility      : constant Facilities.Handle :=
           new Facilities.File_Descriptor.Instance;
         Facility_Name : constant String            :=
           "Test_Facility";
      begin
         Facility.Set_Name (Name => Facility_Name);
         Facility.Toggle_Write_Timestamp (State => False);
         Assert (Condition => not Facility.Is_Write_Timestamp,
                 Message   => "Could not disable Timestamp");

         Log.Attach_Facility (Facility => Facility);
         Log.Update (Name    => Facility_Name,
                     Process => Enable_Facility_Timestamp'Access);

         --  Since Update is not synchronous and we are accessing the facility
         --  directly we must wait for the update to actually take place.

         delay 0.1;

         Assert (Condition => Facility.Is_Write_Timestamp,
                 Message   => "Update failed");
      end;
   end Update_Facility;

   -------------------------------------------------------------------------

   procedure Verify_Logger_Initialization is
      Logger1 : Tasked_Logger.Instance;
      Logger2 : Tasked_Logger.Instance (Init => True);
      F_Count : Natural := Natural'Last;
   begin

      Logger1.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 0,
              Message   => "logger1 not empty");

      Logger2.Facility_Count (Count => F_Count);
      Assert (Condition => F_Count = 1,
              Message   => "logger2 empty");
   end Verify_Logger_Initialization;

end Tasked_Logger_Tests;
