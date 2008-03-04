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
with Ada.IO_Exceptions;
with Ada.Exceptions;
--  Ahven
with Ahven;
use Ahven;
--  Alog
with Alog;
with Alog.Logger;
with Alog.Helpers;
with Alog.Facilities;
use Alog.Facilities;
with Alog.Facilities.File_Descriptor;

package body Logger_Tests is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out L_Test) is
   begin
      Set_Name (T, "Tests for Alog Logger");
      Ahven.Framework.Add_Test_Routine
        (T, Attach_A_Facility'Access,
         "attach a facility");
      Ahven.Framework.Add_Test_Routine
        (T, Detach_A_Facility'Access,
         "detach a facility");
      Ahven.Framework.Add_Test_Routine
        (T, Clear_A_Logger'Access,
         "clear logger");
      Ahven.Framework.Add_Test_Routine
        (T, Log_One_FD_Facility'Access,
         "log to one fd facility");
      Ahven.Framework.Add_Test_Routine
        (T, Log_Multiple_FD_Facilities'Access,
         "log to multiple fd facilities");
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (T : in out L_Test) is
      use Ada.Text_IO;
      use Ahven.Framework;

      --  Files to clean after tests.
      subtype Count is Natural range 1 .. 3;

      Files : array (Count) of BS_Path.Bounded_String :=
        (BS_Path.To_Bounded_String ("./data/Log_One_FD_Facility"),
         BS_Path.To_Bounded_String ("./data/Log_Multiple_FD_Facilities1"),
         BS_Path.To_Bounded_String ("./data/Log_Multiple_FD_Facilities2")
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

   procedure Attach_A_Facility is
      Logger   : Alog.Logger.Instance;
      Facility : Alog.Facilities.Handle :=
        new Alog.Facilities.File_Descriptor.Instance;
   begin
      Logger.Attach_Facility (Facility => Facility);
      Assert (Condition => Logger.Facility_Count = 1,
              Message => "could not attach facility");
   end Attach_A_Facility;

   ---------------------
   -- Detach_Facility --
   ---------------------

   procedure Detach_A_Facility is
   begin
      Fail ("not yet implemented!");
   end Detach_A_Facility;

   --------------------
   -- Clear_A_Logger --
   --------------------

   procedure Clear_A_Logger is
      Logger   : Alog.Logger.Instance;
      Facility : Alog.Facilities.Handle :=
        new Alog.Facilities.File_Descriptor.Instance;
   begin
      Logger.Attach_Facility (Facility => Facility);
      Assert (Condition => Logger.Facility_Count = 1,
              Message   => "could not attach facility");

      --  Clear it.
      Logger.Clear;
      Assert (Condition => Logger.Facility_Count = 0,
              Message   => "could not clear logger");
   end Clear_A_Logger;

   -------------------------
   -- Log_One_FD_Facility --
   -------------------------

   procedure Log_One_FD_Facility is
      Logger   : Alog.Logger.Instance;
      Facility : Alog.Facilities.Handle :=
        new Alog.Facilities.File_Descriptor.Instance;
      Testfile : String := "./data/Log_One_FD_Facility";
      Reffile  : String := "./data/Log_One_FD_Facility.ref";
   begin
      --  Call facility fd specific procedures.
      Alog.Facilities.File_Descriptor.Handle
        (Facility).Toggle_Write_Timestamp (Set => False);
      Alog.Facilities.File_Descriptor.Handle
        (Facility).Set_Logfile (Testfile);

      Logger.Attach_Facility (Facility => Facility);
      Logger.Log_Message (Level => Alog.DEBUG,
                          Msg   => "Logger testmessage, one fd facility");

      --  Cleanup
      Logger.Clear;

      Assert (Condition => Alog.Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "files are not equal");
   end Log_One_FD_Facility;

   --------------------------------
   -- Log_Multiple_FD_Facilities --
   --------------------------------

   procedure Log_Multiple_FD_Facilities is
      Logger    : Alog.Logger.Instance;

      Facility1 : Alog.Facilities.Handle :=
        new Alog.Facilities.File_Descriptor.Instance;
      Testfile1 : String := "./data/Log_Multiple_FD_Facilities1";
      Reffile1  : String := "./data/Log_Multiple_FD_Facilities1.ref";

      Facility2 : Alog.Facilities.Handle :=
        new Alog.Facilities.File_Descriptor.Instance;
      Testfile2 : String := "./data/Log_Multiple_FD_Facilities2";
      Reffile2  : String := "./data/Log_Multiple_FD_Facilities2.ref";
   begin
      --  Set unique names.
      Facility1.Set_Name (Name => "Facility1");
      Facility2.Set_Name (Name => "Facility2");

      --  Call facility fd specific procedures.
      Alog.Facilities.File_Descriptor.Handle
        (Facility1).Toggle_Write_Timestamp (Set => False);
      Alog.Facilities.File_Descriptor.Handle
        (Facility1).Set_Logfile (Testfile1);

      Alog.Facilities.File_Descriptor.Handle
        (Facility2).Toggle_Write_Timestamp (Set => False);
      Alog.Facilities.File_Descriptor.Handle
        (Facility2).Set_Logfile (Testfile2);

      --  Set INFO-threshold for second facility.
      Facility2.Set_Threshold (Level => Alog.INFO);

      --  Attach both facilities to logger instance.
      Logger.Attach_Facility (Facility => Facility1);
      Logger.Attach_Facility (Facility => Facility2);

      --  Log two messages with different loglevels.
      Logger.Log_Message (Level => Alog.DEBUG,
                          Msg   => "Logger testmessage, multiple facilities");
      Logger.Log_Message (Level => Alog.INFO,
                          Msg   => "Logger testmessage, multiple facilities");

      --  Cleanup
      Logger.Clear;

      Assert (Condition => Alog.Helpers.Assert_Files_Equal
              (Filename1 => Reffile1,
               Filename2 => Testfile1),
              Message   => "file1 is not equal");

      Assert (Condition => Alog.Helpers.Assert_Files_Equal
              (Filename1 => Reffile2,
               Filename2 => Testfile2),
              Message   => "file2 is not equal");
   end Log_Multiple_FD_Facilities;

end Logger_Tests;
