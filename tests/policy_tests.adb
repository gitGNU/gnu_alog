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

with Alog.Maps;
with Alog.Policy_DB;

package body Policy_Tests is

   use Ahven;
   use Alog;

   package DB renames Alog.Policy_DB;

   -------------------------------------------------------------------------

   procedure Default_Loglevel_Handling is
      Before    : constant Log_Level := DB.Get_Default_Loglevel;
      New_Level : constant Log_Level := Critical;
   begin
      DB.Set_Default_Loglevel (Level => New_Level);
      Assert (Condition => DB.Get_Default_Loglevel = New_Level,
              Message   => "Default level mismatch");

      DB.Set_Default_Loglevel (Level => Before);
   end Default_Loglevel_Handling;

   -------------------------------------------------------------------------

   procedure Finalize (T : in out Testcase) is
      pragma Unreferenced (T);
   begin
      DB.Reset;
   end Finalize;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for the logging policy database");
      T.Add_Test_Routine (Routine => Reset_Policy_DB'Access,
                          Name    => "reset policy database");
      T.Add_Test_Routine (Routine => Default_Loglevel_Handling'Access,
                          Name    => "default loglevel handling");
      T.Add_Test_Routine (Routine => Src_Loglevel_Handling'Access,
                          Name    => "source loglevel handling");
      T.Add_Test_Routine (Routine => Set_Sources_Map'Access,
                          Name    => "set source map");
      T.Add_Test_Routine (Routine => Verify_Accept_Src'Access,
                          Name    => "accept source");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Reset_Policy_DB is
   begin
      DB.Set_Default_Loglevel (Level => Error);
      DB.Set_Src_Loglevel (Source => "Foo",
                           Level  => Warning);

      DB.Reset;

      Assert (Condition => DB.Get_Default_Loglevel = DB.Alog_Default_Level,
              Message   => "Default loglevel mismatch");

      declare
         Src_Level : Log_Level;
         pragma Unreferenced (Src_Level);
      begin
         Src_Level := DB.Get_Src_Loglevel (Source => "Foo");
         Fail (Message => "Src levels not reset");

      exception
         when DB.No_Source_Loglevel =>
            null;
      end;
   end Reset_Policy_DB;

   -------------------------------------------------------------------------

   procedure Set_Sources_Map is
      Map : Maps.Wildcard_Level_Map;
   begin
      Map.Insert (Key  => "Foo",
                  Item => Notice);
      Map.Insert (Key  => "Bar",
                  Item => Warning);

      DB.Set_Src_Loglevel (Sources => Map);

      Assert (Condition => DB.Get_Src_Loglevel (Source => "Foo") = Notice,
              Message   => "Foo source loglevel mismatch");
      Assert (Condition => DB.Get_Src_Loglevel (Source => "Bar") = Warning,
              Message   => "Bar source loglevel mismatch");
   end Set_Sources_Map;

   -------------------------------------------------------------------------

   procedure Src_Loglevel_Handling is
   begin
      DB.Set_Src_Loglevel (Source => "Foo",
                           Level  => Info);
      Assert (Condition => DB.Get_Src_Loglevel (Source => "Foo") = Info,
              Message   => "Source loglevel mismatch");

      DB.Set_Src_Loglevel (Source => "Foo",
                           Level  => Error);
      Assert (Condition => DB.Get_Src_Loglevel (Source => "Foo") = Error,
              Message   => "Unable to update source loglevel");

      declare
         Level : Log_Level;
         pragma Unreferenced (Level);
      begin
         Level := DB.Get_Src_Loglevel (Source => "Bar");
         Fail (Message => "Expected No_Source_Loglevel");

      exception
         when DB.No_Source_Loglevel =>
            null;
      end;
   end Src_Loglevel_Handling;

   -------------------------------------------------------------------------

   procedure Verify_Accept_Src is
   begin
      DB.Reset;

      DB.Set_Default_Loglevel (Level => Info);

      Assert (Condition => not DB.Accept_Src (Level => Debug),
              Message   => "Debug accepted");
      Assert (Condition => DB.Accept_Src (Level => Warning),
              Message   => "Warning not accepted");

      DB.Set_Src_Loglevel (Source => "Foo.*",
                           Level  => Error);
      Assert (Condition => not DB.Accept_Src
              (Source => "Foo",
               Level  => Debug),
              Message   => "Src debug accepted");
      Assert (Condition => DB.Accept_Src
              (Source => "Foo",
               Level  => Critical),
              Message   => "Src critical not accepted");
      Assert (Condition => DB.Accept_Src
              (Source => "Bar",
               Level  => Info),
              Message   => "Src bar not accepted");
   end Verify_Accept_Src;

end Policy_Tests;
