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

with Ada.Task_Identification;
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;

package body Alog.Logger.Tasking is

   use Ada.Exceptions;

   function "<" (Left, Right : Ada.Task_Identification.Task_Id) return Boolean;
   --  Smaller-than function for Task_Id. Needed to use Task_Id as Key_Type for
   --  Ordered_Map.

   package Map_Of_Exception_Occurrences is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Ada.Task_Identification.Task_Id,
        Element_Type => Ada.Exceptions.Exception_Occurrence_Access);

   package MOEO renames Map_Of_Exception_Occurrences;
   --  Per-task Exception_Occurrence storage. This map works like a message box
   --  for exception occurrences which are stored on a per-caller (Task_Id)
   --  basis. Exception_Occurrences can be inquired by callers via the
   --  Get_Last_Occurrence entry.

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Ada.Exceptions.Exception_Occurrence,
      Name   => Ada.Exceptions.Exception_Occurrence_Access);
   --  Free memory allocated by an Exception_Occurrence.

   -------------------------------------------------------------------------

   function "<" (Left, Right : Ada.Task_Identification.Task_Id) return Boolean
   is
      use Ada.Task_Identification;
   begin
      return Image (T => Left) < Image (T => Right);
   end "<";

   -------------------------------------------------------------------------

   task body Instance is
      Logsink               : Logger.Instance (Init => Init);
      Current_Level         : Log_Level;
      Current_Message       : Unbounded_String;
      Current_Caller        : Ada.Task_Identification.Task_Id;
      Exception_Occurrences : MOEO.Map;
   begin

      loop
         begin

            select

               ----------------------------------------------------------------

               accept Attach_Facility (Facility : Facilities.Handle) do
                  Logsink.Attach_Facility (Facility => Facility);
               end Attach_Facility;
            or

               ----------------------------------------------------------------

               accept Detach_Facility (Name : String) do
                  Logsink.Detach_Facility (Name => Name);
               end Detach_Facility;
            or

               ----------------------------------------------------------------

               accept Facility_Count (Count : out Natural) do
                  Count := Logsink.Facility_Count;
               end Facility_Count;
            or

               ----------------------------------------------------------------

               accept Clear do
                  Logsink.Clear;
               end Clear;
            or

               ----------------------------------------------------------------

               accept Get_Facility (Name     : String;
                                    Facility : out Facilities.Handle)
               do
                  Facility := Logsink.Get_Facility (Name => Name);
               end Get_Facility;
            or

               ----------------------------------------------------------------

               accept Get_Last_Exception
                 (Occurrence : out Exception_Occurrence)
               do
                  declare
                     use MOEO;

                     Position : Cursor;
                  begin
                     Position := Exception_Occurrences.Find
                       (Key => Get_Last_Exception'Caller);

                     if Position = No_Element then
                        Save_Occurrence (Target => Occurrence,
                                         Source => Null_Occurrence);
                     else
                        Save_Occurrence
                          (Target => Occurrence,
                           Source => MOEO.Element (Position => Position).all);
                     end if;
                  end;
               end Get_Last_Exception;
            or

               ----------------------------------------------------------------

               accept Log_Message
                 (Level : Log_Level;
                  Msg   : String)
               do
                  Current_Level   := Level;
                  Current_Message := To_Unbounded_String (Msg);
                  Current_Caller  := Instance.Log_Message'Caller;
               end Log_Message;

               declare
                  use MOEO;

                  Position : Cursor;
               begin
                  Position := Exception_Occurrences.Find
                    (Key => Current_Caller);

                  if Position /= No_Element then
                     declare
                        Handle : Ada.Exceptions.Exception_Occurrence_Access;
                     begin
                        Handle := MOEO.Element (Position => Position);
                        Free (Handle);
                        Exception_Occurrences.Delete (Position => Position);
                     end;
                  end if;

                  Logsink.Log_Message (Level => Current_Level,
                                       Msg   => To_String (Current_Message));

               exception
                  when E : others =>
                     Exception_Occurrences.Insert
                       (Key      => Current_Caller,
                        New_Item => Save_Occurrence (Source => E));
               end;
            or

               terminate;
            end select;

            --  Exceptions raised during a rendezvous are raised here and in the
            --  calling task. Catch and ignore it so the tasked logger does not
            --  get terminated after an exception.

         exception
            when others =>
               null;
         end;
      end loop;
   end Instance;

end Alog.Logger.Tasking;
