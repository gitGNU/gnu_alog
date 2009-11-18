--
--  Copyright (c) 2009,
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

with Ahven;

with Alog.Helpers;

package body Helper_Tests is

   use Ahven;
   use Alog.Helpers;

   -------------------------------------------------------------------------

   procedure Compare_Files is
      File1 : constant String := "./data/ref_file1";
      File2 : constant String := "./data/ref_file2";
      File3 : constant String := "./data/ref_file3";
      File4 : constant String := "./data/ref_file4";
   begin
      Assert
        (Condition => Assert_Files_Equal
           (Filename1 => File1,
            Filename2 => File2),
         Message   => "files should be equal");

      Assert
        (Condition => not Assert_Files_Equal
           (Filename1 => File1,
            Filename2 => File3),
         Message   => "files should not be equal");

      Assert
        (Condition => not Assert_Files_Equal
           (Filename1 => File1,
            Filename2 => File4),
         Message   => "filesize should not be equal");
   end Compare_Files;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for Alog helpers");
      T.Add_Test_Routine
        (Routine => Compare_Files'Access,
         Name    => "test file equality helper");
      T.Add_Test_Routine
        (Routine => Wildcard_Stripping'Access,
         Name    => "wildcard stripping");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Wildcard_Stripping is
      Off_String : constant String (5 .. 11) := "Foo.Bar";
   begin
      Assert (Condition => Wildcard_Strip (Input => "Foo") = "",
              Message   => "empty string expected");

      Assert (Condition => Wildcard_Strip
              (Input => "Foo.Bar.Foo") = "Foo.Bar.*",
              Message   => "Foo.Bar.* expected");

      Assert (Condition => Wildcard_Strip
              (Input => "Foo.Bar") = "Foo.*",
              Message   => "Foo.* expected");

      Assert (Condition => Wildcard_Strip (Input => Off_String) = "Foo.*",
              Message   => "Foo.* expected (offstring)");
   end Wildcard_Stripping;

end Helper_Tests;
