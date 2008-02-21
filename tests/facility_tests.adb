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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO;
use Ada;

--  Ahven
with Ahven; use Ahven;

--  Alog
with Alog; use Alog;
with Alog.Helpers;
with Alog.Facilities.File_Descriptor; use Alog.Facilities;

package body Facility_Tests is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out FTest) is
   begin
      Set_Name (T, "Tests for Alog Facilites");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Name'Access, "set facility name");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Name_too_Long'Access, "set overlength name");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Valid_Logfile_Fd'Access, "set valid logfile");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Invalid_Logfile_Fd'Access, "set invalid logfile");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Threshold'Access, "set threshold");
      Ahven.Framework.Add_Test_Routine
        (T, Write_Message_Fd'Access, "log a message");
      Ahven.Framework.Add_Test_Routine
        (T, Teardown_Fd'Access, "teardown FD facility");
      Ahven.Framework.Add_Test_Routine
        (T, Disable_Write_Timestamp_Fd'Access, "disable timestamp");
      Ahven.Framework.Add_Test_Routine
        (T, Disable_Write_Loglevel_Fd'Access, "disable loglevel");
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (T : in out FTest) is
      use Ada.Text_IO;
      use Ahven.Framework;

      Files : String := ("./data/Teardown_Fd");
      --  Files to clean after tests.
      F     : File_Type;
   begin
      Open (File => F, Mode => In_File, Name => Files);
      Delete (File => F);

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

   -----------------------
   -- Set_Name_too_Long --
   -----------------------

   procedure Set_Name_too_Long is
      use Ada.Strings;
      F        : File_Descriptor.Instance;
      Expected : String := "NAMETOOLONG";
   begin
      F.Set_Name (Name => Expected);
      Fail ("no exception raised!");
   exception
      when Length_Error =>
         Assert (Condition => True,
                 Message => "expected exception occured!");
   end Set_Name_too_Long;

   -------------------
   -- Set_Threshold --
   -------------------

   procedure Set_Threshold is
      F        : File_Descriptor.Instance;
      Expected : Log_Level := DEBUG;
   begin
      F.Set_Threshold (Expected);
      Assert (Condition => F.Get_Threshold = Expected,
             Message => "Log_Level not equal");
   end Set_Threshold;

   -----------------------
   -- Set_Valid_Logfile --
   -----------------------

   procedure Set_Valid_Logfile_Fd is
      use Ada.Text_IO;
      F : File_Descriptor.Instance;
   begin
      F.Set_Logfile (Path => "./data/Set_Valid_Logfile");
      Assert (Condition => Is_Open (F.Get_Logfile),
              Message   => "could not set logfile!");
      F.Close_Logfile (Remove => True);
   end Set_Valid_Logfile_Fd;

   --------------------
   -- Set_Illegal_Fd --
   --------------------

   procedure Set_Invalid_Logfile_Fd is
      use Ada.IO_Exceptions;

      F : File_Descriptor.Instance;
   begin
      F.Set_Logfile (Path => "/not/allowed.log");
      Fail (Message => "no exception raised!");
   exception
      when Name_Error =>
         Assert (Condition => True,
                 Message => "expected exception occured!");
   end Set_Invalid_Logfile_Fd;

   ----------------------
   -- Write_Message_Fd --
   ----------------------

   procedure Write_Message_Fd is
      F        : File_Descriptor.Instance;
      Testfile : String := "./data/Write_Message_Fd";
      Reffile  : String := "./data/Write_Message_Fd.ref";
   begin
      --  We have to disable timestamps, since its changing all
      --  the time :)
      F.Toggle_Write_Timestamp (Set => False);

      --  Open logfile, write test message.
      F.Set_Logfile (Path => Testfile);
      F.Write_Message (Msg => "This is a test log-message");

      F.Close_Logfile;

      --  Compare both files.
      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile, Filename2 => Testfile),
              Message   => "files not equal");

      --  Cleanup
      F.Teardown;
   end Write_Message_Fd;

   -----------------
   -- Teardown_Fd --
   -----------------

   procedure Teardown_Fd is
      use Ada.Text_IO;
      F : File_Descriptor.Instance;
   begin
      F.Set_Logfile (Path => "./data/Teardown_Fd");
      Assert (Condition => Is_Open (File => F.Get_Logfile),
              Message   => "could not set logfile!");
      F.Teardown;
      Assert (Condition => not Is_Open (File => F.Get_Logfile),
              Message   => "logfile still open!");
   end Teardown_Fd;

   --------------------------------
   -- Disable_Write_Timestamp_Fd --
   --------------------------------

   procedure Disable_Write_Timestamp_Fd is
      F : File_Descriptor.Instance;
   begin
      F.Toggle_Write_Timestamp (Set => False);
      Assert (Condition => not F.Is_Write_Timestamp,
              Message   => "unable to disable timestamp");
   end Disable_Write_Timestamp_Fd;

   -------------------------------
   -- Disable_Write_Loglevel_Fd --
   -------------------------------

   procedure Disable_Write_Loglevel_Fd is
      F : File_Descriptor.Instance;
   begin
      F.Toggle_Write_Loglevel (Set => False);
      Assert (Condition => not F.Is_Write_Loglevel,
              Message   => "unable to disable loglevel");
   end Disable_Write_Loglevel_Fd;

end Facility_Tests;
