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

with Ada.Strings.Unbounded;

with Alog.Transforms.Casing;

package body Transform_Tests.Casing is

   use Alog;
   use Ada.Strings.Unbounded;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out T_Test) is
   begin
      Set_Name (T, "Tests for Alog Transform Casing");
      Ahven.Framework.Add_Test_Routine
        (T, Transform_Message_Lowercase'Access,
         "transform a message to lowercase");
      Ahven.Framework.Add_Test_Routine
        (T, Transform_Message_Uppercase'Access,
         "transform a message to uppercase");
   end Initialize;

   ---------------------------------
   -- Transform_Message_Lowercase --
   ---------------------------------

   procedure Transform_Message_Lowercase is
      T       : Alog.Transforms.Casing.Instance;
      Message : constant String := "Test Message";
      Transformed_Msg : Unbounded_String;
      Ref_Transformed_Msg : constant String := "test message";
   begin
      --  Initialize
      T.Setup;
      Transformed_Msg := To_Unbounded_String (
                           T.Transform_Message (INFO, Message));
      Assert (Condition => Transformed_Msg = Ref_Transformed_Msg,
              Message   => "Output does not match expected value (lowercase)");
      --  Cleanup
      T.Teardown;
   end Transform_Message_Lowercase;

   ---------------------------------
   -- Transform_Message_Uppercase --
   ---------------------------------

   procedure Transform_Message_Uppercase is
      T       : Alog.Transforms.Casing.Instance;
      Message : constant String := "Test Message";
      Transformed_Msg     : Unbounded_String;
      Ref_Transformed_Msg : constant String := "TEST MESSAGE";
   begin
      --  Initialize
      T.Setup;
      T.Set_Mode (Alog.Transforms.Casing.Uppercase);
      Transformed_Msg := To_Unbounded_String (
                           T.Transform_Message (INFO, Message));
      Assert (Condition => Transformed_Msg = Ref_Transformed_Msg,
              Message   => "Output does not match expected value (uppercase)");
      --  Cleanup
      T.Teardown;
   end Transform_Message_Uppercase;

end Transform_Tests.Casing;
