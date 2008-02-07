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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Text_IO;
with Ahven; use Ahven;
with Alog; use Alog;
with Alog.Facilities.File_Descriptor;
use Alog.Facilities.File_Descriptor;

package body Facility_Tests is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out FTest) is
   begin
      Set_Name (T, "Tests for Alog Facilites");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Name'Access, "Set Facility Name");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Name_too_Long'Access, "Set overlength Name");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Valid_Logfile'Access, "Set valid Logfile");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Invalid_Logfile'Access, "Set invalid Logfile");
      Ahven.Framework.Add_Test_Routine
        (T, Set_Threshold'Access, "Set Threshold");
      Ahven.Framework.Add_Test_Routine
        (T, Log_Message_Fd'Access, "Log a Message");
      Ahven.Framework.Add_Test_Routine
        (T, Teardown_Fd'Access, "Teardown Fd Facility");
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (T : in out FTest) is
      use Ada.Text_IO;
      use Ahven.Framework;

      Files : String := "./data/Teardown_Fd";
      --  Atm. we have only one file to clean.
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
         Put_Line ("Error occured while cleaning up: ");
         Put_Line (Ada.Exceptions.Exception_Name (Event));
         Put_Line (Ada.Exceptions.Exception_Message (Event));
   end Finalize;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name is
      F        : Facility_Fd;
      Expected : String := "TEST";
   begin
      F.Set_Name (Name => Expected);
      Assert (Condition => F.Get_Name = Expected,
             Message => "Name not equal");
   end Set_Name;

   -----------------------
   -- Set_Name_too_Long --
   -----------------------

   procedure Set_Name_too_Long is
      use Ada.Strings;
      F        : Facility_Fd;
      Expected : String := "NAMETOOLONG";
   begin
      F.Set_Name (Name => Expected);
      Fail ("No exception raised!");
   exception
      when Length_Error =>
         Assert (Condition => True,
                 Message => "Expected exception occured!");
   end Set_Name_too_Long;

   -----------------------
   -- Set_Valid_Logfile --
   -----------------------

   procedure Set_Valid_Logfile is
      use Ada.Text_IO;
      F : Facility_Fd;
   begin
      F.Set_Logfile (Path => "./data/Set_Valid_Logfile");
      Assert (Condition => Is_Open (F.Get_Logfile),
              Message   => "Could not set logfile!");
      F.Close_Logfile (Remove => True);
   end Set_Valid_Logfile;

   --------------------
   -- Set_Illegal_Fd --
   --------------------

   procedure Set_Invalid_Logfile is
      F : Facility_Fd;
   begin
      F.Set_Logfile (Path => "/not/allowed.log");
      Fail (Message => "No exception raised!");
   exception
      when Name_Error =>
         Assert (Condition => True,
                 Message => "Expected exception occured!");
   end Set_Invalid_Logfile;

   -------------------
   -- Set_Threshold --
   -------------------

   procedure Set_Threshold is
      F        : Facility_Fd;
      Expected : Log_Level := DEBUG;
   begin
      F.Set_Threshold (Expected);
      Assert (Condition => F.Get_Threshold = Expected,
             Message => "Log_Level not equal");
   end Set_Threshold;

   --------------------
   -- Log_Message_Fd --
   --------------------

   procedure Log_Message_Fd is
   begin
      Fail (Message => "Not yet implemented!");
   end Log_Message_Fd;

   -----------------
   -- Teardown_Fd --
   -----------------

   procedure Teardown_Fd is
      use Ada.Text_IO;
      F : Facility_Fd;
   begin
      F.Set_Logfile (Path => "./data/Teardown_Fd");
      Assert (Condition => Is_Open (File => F.Get_Logfile),
              Message   => "Could not set logfile!");
      F.Teardown;
      Assert (Condition => not Is_Open (File => F.Get_Logfile),
              Message   => "Logfile still open!");
   end Teardown_Fd;

end Facility_Tests;
