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
      T.Add_Test_Routine
        (Routine => Reset_Policy_DB'Access,
         Name    => "reset policy database");
      T.Add_Test_Routine
        (Routine => Default_Loglevel_Handling'Access,
         Name    => "default loglevel handling");
      T.Add_Test_Routine
        (Routine => Src_Loglevel_Handling'Access,
         Name    => "source loglevel handling");
      T.Add_Test_Routine
        (Routine => Set_Sources_Map'Access,
         Name    => "set source map");
      T.Add_Test_Routine
        (Routine => Verify_Accept_Src'Access,
         Name    => "accept source");
      T.Add_Test_Routine
        (Routine => Lookup_Src'Access,
         Name    => "lookup source");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Lookup_Src is
   begin
      DB.Set_Default_Loglevel (Level => Error);
      DB.Set_Loglevel (Identifier => "Lookup.*",
                       Level      => Warning);

      Assert (Condition => DB.Lookup
              (Identifier => "Lookup") = Warning,
              Message   => "Lookup mismatch");
      Assert (Condition => DB.Lookup
              (Identifier => "Nonexistent") = Error,
              Message   => "Nonexistent mismatch");
   end Lookup_Src;

   -------------------------------------------------------------------------

   procedure Reset_Policy_DB is
   begin
      DB.Set_Default_Loglevel (Level => Error);
      DB.Set_Loglevel (Identifier => "Foo",
                       Level      => Warning);

      DB.Reset;

      Assert (Condition => DB.Get_Default_Loglevel = DB.Alog_Default_Level,
              Message   => "Default loglevel mismatch");

      declare
         Src_Level : Log_Level;
         pragma Unreferenced (Src_Level);
      begin
         Src_Level := DB.Get_Loglevel (Identifier => "Foo");
         Fail (Message => "Src levels not reset");

      exception
         when DB.No_Ident_Loglevel =>
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

      DB.Set_Loglevel (Identifiers => Map);

      Assert (Condition => DB.Get_Loglevel (Identifier => "Foo") = Notice,
              Message   => "Foo source loglevel mismatch");
      Assert (Condition => DB.Get_Loglevel (Identifier => "Bar") = Warning,
              Message   => "Bar source loglevel mismatch");
   end Set_Sources_Map;

   -------------------------------------------------------------------------

   procedure Src_Loglevel_Handling is
   begin
      DB.Set_Loglevel (Identifier => "Foo",
                       Level      => Info);
      Assert (Condition => DB.Get_Loglevel (Identifier => "Foo") = Info,
              Message   => "Source loglevel mismatch");

      DB.Set_Loglevel (Identifier => "Foo",
                       Level      => Error);
      Assert (Condition => DB.Get_Loglevel (Identifier => "Foo") = Error,
              Message   => "Unable to update source loglevel");

      declare
         Level : Log_Level;
         pragma Unreferenced (Level);
      begin
         Level := DB.Get_Loglevel (Identifier => "Bar");
         Fail (Message => "Expected No_Ident_Loglevel");

      exception
         when DB.No_Ident_Loglevel =>
            null;
      end;
   end Src_Loglevel_Handling;

   -------------------------------------------------------------------------

   procedure Verify_Accept_Src is
   begin
      DB.Reset;

      DB.Set_Default_Loglevel (Level => Info);

      Assert (Condition => not DB.Accept_Ident (Level => Debug),
              Message   => "Debug accepted");
      Assert (Condition => DB.Accept_Ident (Level => Warning),
              Message   => "Warning not accepted");

      DB.Set_Loglevel (Identifier => "Foo.*",
                       Level      => Error);
      Assert (Condition => not DB.Accept_Ident
              (Identifier => "Foo",
               Level      => Debug),
              Message   => "Src debug accepted");
      Assert (Condition => DB.Accept_Ident
              (Identifier => "Foo",
               Level      => Critical),
              Message   => "Src critical not accepted");
      Assert (Condition => DB.Accept_Ident
              (Identifier => "Bar",
               Level      => Info),
              Message   => "Src bar not accepted");

      DB.Set_Loglevel (Identifier => "Foobar.*",
                       Level      => Debug);
      Assert (Condition => DB.Accept_Ident
              (Identifier => "Foobar",
               Level      => Debug),
              Message   => "Src foobar not accepted");
   end Verify_Accept_Src;

end Policy_Tests;
