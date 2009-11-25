--
--  Copyright (c) 2008-2009,
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

with Ada.Text_IO;
with Ada.Directories;
with Ada.IO_Exceptions;

with Ahven; use Ahven;

with Alog.Helpers;
with Alog.Facilities.File_Descriptor;

package body Facility_Tests.FD is

   use Alog;
   use Alog.Facilities;

   -------------------------------------------------------------------------

   procedure Disable_Write_Loglevel_Fd is
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Disable_Write_Loglevel_Fd";
      Reffile  : constant String := "./data/Disable_Write_Loglevel_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (State => False);
      F.Toggle_Write_Loglevel (State => False);
      F.Set_Logfile (Path => Testfile);
      F.Log_Message (Msg => "This is a message without loglevel");

      F.Close_Logfile;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "unable to disable");

      Ada.Directories.Delete_File (Name => Testfile);
      F.Teardown;
   end Disable_Write_Loglevel_Fd;

   -------------------------------------------------------------------------

   procedure Disable_Write_Timestamp_Fd is
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Disable_Write_Timestamp_Fd";
      Reffile  : constant String := "./data/Disable_Write_Timestamp_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (State => False);
      F.Set_Logfile (Path => Testfile);
      F.Log_Message (Msg => "This is a message without timestamp");

      F.Close_Logfile;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "unable to disable");

      Ada.Directories.Delete_File (Name => Testfile);
      F.Teardown;
   end Disable_Write_Timestamp_Fd;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for Alog Facility FD");
      T.Add_Test_Routine
        (Routine => Set_Valid_Logfile_Fd'Access,
         Name    => "set valid logfile");
      T.Add_Test_Routine
        (Routine => Set_Invalid_Logfile_Fd'Access,
         Name    => "set invalid logfile");
      T.Add_Test_Routine
        (Routine => Write_Message_Fd'Access,
         Name    => "log a fd message");
      T.Add_Test_Routine
        (Routine => Teardown_Fd'Access,
         Name    => "teardown fd facility");
      T.Add_Test_Routine
        (Routine => Disable_Write_Timestamp_Fd'Access,
         Name    => "disable fd timestamp");
      T.Add_Test_Routine
        (Routine => Disable_Write_Loglevel_Fd'Access,
         Name    => "disable fd loglevel");
      T.Add_Test_Routine
        (Routine => Trim_Loglevels_Fd'Access,
         Name    => "fd loglevel align");
      T.Add_Test_Routine
        (Routine => Set_Threshold_Fd'Access,
         Name    => "set fd threshold");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Set_Invalid_Logfile_Fd is
      use Ada.IO_Exceptions;

      F : File_Descriptor.Instance;
   begin
      F.Set_Logfile (Path => "/not/allowed.log");
      Fail (Message => "no exception raised!");
   exception
      when Name_Error =>
         Assert (Condition => True,
                 Message   => "expected exception occured!");
   end Set_Invalid_Logfile_Fd;

   -------------------------------------------------------------------------

   procedure Set_Threshold_Fd is
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Set_Threshold_Fd";
      Reffile  : constant String := "./data/Set_Threshold_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (State => False);
      F.Toggle_Write_Loglevel (State => True);
      F.Set_Logfile (Path => Testfile);
      F.Log_Message (Level => Debug,
                     Msg   => "this message should appear in log");
      F.Set_Threshold (Level => Info);
      F.Log_Message (Level => Debug,
                     Msg   => "this message should not appear");
      F.Log_Message (Level => Info,
                     Msg   => "this message should appear again");

      F.Close_Logfile;
      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile, Filename2 => Testfile),
              Message   => "threshold does not work");

      Ada.Directories.Delete_File (Name => Testfile);
      F.Teardown;
   end Set_Threshold_Fd;

   -------------------------------------------------------------------------

   procedure Set_Valid_Logfile_Fd is
      use Ada.Text_IO;
      F : File_Descriptor.Instance;
   begin
      F.Set_Logfile (Path => "./data/Set_Valid_Logfile");
      Assert (Condition => Is_Open (F.Get_Logfile.all),
              Message   => "could not set logfile!");
      F.Close_Logfile (Remove => True);
   end Set_Valid_Logfile_Fd;

   -------------------------------------------------------------------------

   procedure Teardown_Fd is
      use Ada.Text_IO;
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Teardown_Fd";
   begin
      F.Set_Logfile (Path => Testfile);
      Assert (Condition => Is_Open (File => F.Get_Logfile.all),
              Message   => "could not set logfile!");
      F.Teardown;
      Assert (Condition => not Is_Open (File => F.Get_Logfile.all),
              Message   => "logfile still open!");

      Ada.Directories.Delete_File (Name => Testfile);
   end Teardown_Fd;

   -------------------------------------------------------------------------

   procedure Trim_Loglevels_Fd is
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Trim_Loglevels_Fd";
      Reffile  : constant String := "./data/Trim_Loglevels_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (State => False);
      F.Toggle_Write_Loglevel (State => True);
      F.Set_Logfile (Path => Testfile);
      for Lvl in Alog.Log_Level loop
         F.Log_Message (Level => Lvl,
                        Msg   => "Testmessage");
      end loop;

      F.Close_Logfile;
      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile, Filename2 => Testfile),
              Message   => "alignment incorrect");

      Ada.Directories.Delete_File (Name => Testfile);
      F.Teardown;
   end Trim_Loglevels_Fd;

   -------------------------------------------------------------------------

   procedure Write_Message_Fd is
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Write_Message_Fd";
      Reffile  : constant String := "./data/Write_Message_Fd.ref";
   begin
      --  We have to disable timestamps, since its changing all
      --  the time :)
      F.Toggle_Write_Timestamp (State => False);

      --  Open logfile, write test message.
      F.Set_Logfile (Path => Testfile);
      F.Log_Message (Msg => "This is a test log-message");

      F.Close_Logfile;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "files not equal");

      Ada.Directories.Delete_File (Name => Testfile);
      F.Teardown;
   end Write_Message_Fd;

end Facility_Tests.FD;
