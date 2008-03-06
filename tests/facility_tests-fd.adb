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
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.IO_Exceptions;

--  Ahven
with Ahven; use Ahven;

--  Alog
with Alog;  use Alog;
with Alog.Helpers;
with Alog.Facilities; use Alog.Facilities;
with Alog.Facilities.File_Descriptor;

package body Facility_Tests.FD is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out F_Test) is
   begin
      Set_Name (T, "Tests for Alog Facility FD");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Valid_Logfile_Fd'Access, "set valid logfile");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Invalid_Logfile_Fd'Access, "set invalid logfile");
      Ahven.Framework.Add_Test_Routine
        (T, Write_Message_Fd'Access, "log a fd message");
      Ahven.Framework.Add_Test_Routine
        (T, Teardown_Fd'Access, "teardown fd facility");
      Ahven.Framework.Add_Test_Routine
        (T, Disable_Write_Timestamp_Fd'Access, "disable fd timestamp");
      Ahven.Framework.Add_Test_Routine
        (T, Disable_Write_Loglevel_Fd'Access, "disable fd loglevel");
      Ahven.Framework.Add_Test_Routine
        (T, Trim_Loglevels_Fd'Access, "fd loglevel align");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Threshold_Fd'Access, "set fd threshold");
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (T : in out F_Test) is
      use Ada.Text_IO;
      use Ahven.Framework;

      --  Files to clean after tests.
      subtype Count is Natural range 1 .. 6;

      Files : array (Count) of BS_Path.Bounded_String :=
        (BS_Path.To_Bounded_String ("./data/Teardown_Fd"),
         BS_Path.To_Bounded_String ("./data/Write_Message_Fd"),
         BS_Path.To_Bounded_String ("./data/Disable_Write_Timestamp_Fd"),
         BS_Path.To_Bounded_String ("./data/Disable_Write_Loglevel_Fd"),
         BS_Path.To_Bounded_String ("./data/Trim_Loglevels_Fd"),
         BS_Path.To_Bounded_String ("./data/Set_Threshold_Fd")
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
                 Message   => "expected exception occured!");
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
              (Filename1 => Reffile,
               Filename2 => Testfile),
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
      Testfile : String := "./data/Disable_Write_Timestamp_Fd";
      Reffile  : String := "./data/Disable_Write_Timestamp_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (Set => False);
      F.Set_Logfile (Path => Testfile);
      F.Write_Message (Msg => "This is a message without timestamp");

      F.Close_Logfile;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "unable to disable");

      --  Cleanup.
      F.Teardown;
   end Disable_Write_Timestamp_Fd;

   -------------------------------
   -- Disable_Write_Loglevel_Fd --
   -------------------------------

   procedure Disable_Write_Loglevel_Fd is
      F : File_Descriptor.Instance;
      Testfile : String := "./data/Disable_Write_Loglevel_Fd";
      Reffile  : String := "./data/Disable_Write_Loglevel_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (Set => False);
      F.Toggle_Write_Loglevel (Set => False);
      F.Set_Logfile (Path => Testfile);
      F.Write_Message (Msg => "This is a message without loglevel");

      F.Close_Logfile;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "unable to disable");

      F.Teardown;
   end Disable_Write_Loglevel_Fd;

   -----------------------
   -- Trim_Loglevels_Fd --
   -----------------------

   procedure Trim_Loglevels_Fd is
      F : File_Descriptor.Instance;
      Testfile : String := "./data/Trim_Loglevels_Fd";
      Reffile  : String := "./data/Trim_Loglevels_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (Set => False);
      F.Set_Logfile (Path => Testfile);
      for Lvl in Alog.Log_Level loop
         F.Write_Message (Level => Lvl,
                          Msg   => "Testmessage");
      end loop;

      F.Close_Logfile;
      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile, Filename2 => Testfile),
              Message   => "alignment incorrect");
      F.Teardown;
   end Trim_Loglevels_Fd;

   ----------------------
   -- Set_Threshold_Fd --
   ----------------------

   procedure Set_Threshold_Fd is
      F : File_Descriptor.Instance;
      Testfile : String := "./data/Set_Threshold_Fd";
      Reffile  : String := "./data/Set_Threshold_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (Set => False);
      F.Set_Logfile (Path => Testfile);
      F.Write_Message (Level => DEBU,
                       Msg   => "this message should appear in log");
      F.Set_Threshold (Level => INFO);
      F.Write_Message (Level => DEBU,
                       Msg   => "this message should not appear");
      F.Write_Message (Level => INFO,
                       Msg   => "this message should appear again");

      F.Close_Logfile;
      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile, Filename2 => Testfile),
              Message   => "threshold does not work");

      F.Teardown;
   end Set_Threshold_Fd;

end Facility_Tests.FD;
