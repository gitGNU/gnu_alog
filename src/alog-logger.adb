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

with Alog.Facilities.File_Descriptor;

package body Alog.Logger is

   -------------------------------------------------------------------------

   procedure Attach_Facility (Logger   : in out Alog.Logger.Instance;
                              Facility :        Alog.Facilities.Handle)
   is
   begin
      Logger.F_Stack.Insert (New_Item => Facility);
   end Attach_Facility;

   -------------------------------------------------------------------------

   procedure Attach_Transform (Logger    : in out Instance;
                               Transform :        Alog.Transforms.Handle)
   is
   begin
      Logger.T_Stack.Insert (New_Item => Transform);
   end Attach_Transform;

   -------------------------------------------------------------------------

   procedure Clear (L : in out Instance) is
   begin
      L.Finalize;
   end Clear;

   -------------------------------------------------------------------------

   procedure Detach_Facility (Logger   : in out Instance;
                              Facility :        Alog.Facilities.Handle)
   is
      use Facilities_Stack_Package;

      Position        : Cursor;
      Facility_Handle : Alog.Facilities.Handle;
   begin
      --  Find element first. If not found, exception is raised.
      Position := Logger.F_Stack.Find (Item => Facility);
      Facility_Handle := Element (Position);

      Logger.F_Stack.Delete (Item => Facility);
      --  Free memory.
      Free (Facility_Handle);
   exception
      when Constraint_Error =>
         raise Facility_Not_Found;
   end Detach_Facility;

   -------------------------------------------------------------------------

   procedure Detach_Transform (Logger    : in out Instance;
                               Transform :        Alog.Transforms.Handle)
   is
      use Transforms_Stack_Package;

      Position        : Cursor;
      Transform_Handle : Alog.Transforms.Handle;
   begin
      --  Find element first. If not found, exception is raised.
      Position := Logger.T_Stack.Find (Item => Transform);
      Transform_Handle := Element (Position);

      Logger.T_Stack.Delete (Item => Transform);
      --  Free memory.
      Free (Transform_Handle);
   exception
      when Constraint_Error =>
         raise Transform_Not_Found;
   end Detach_Transform;

   -------------------------------------------------------------------------

   function Facility_Count (Logger : Instance) return Natural is
   begin
      return Natural (Logger.F_Stack.Length);
   end Facility_Count;

   -------------------------------------------------------------------------

   procedure Finalize (Logger : in out Instance) is

      --  Forward specs.
      procedure Free_Facility (Position : Facilities_Stack_Package.Cursor);

      procedure Free_Facility (Position : Facilities_Stack_Package.Cursor)
      is
         use Facilities_Stack_Package;
         Facility_Handle : Alog.Facilities.Handle := Element (Position);
      begin
         --  Cleanup this facility.
         Facility_Handle.Teardown;
         Free (Facility_Handle);
      end Free_Facility;

      --  Forward specs.
      procedure Free_Transform (Position : Transforms_Stack_Package.Cursor);

      procedure Free_Transform (Position : Transforms_Stack_Package.Cursor)
      is
         use Transforms_Stack_Package;
         Transform_Handle : Alog.Transforms.Handle := Element (Position);
      begin
         --  Cleanup this transform.
         Transform_Handle.Teardown;
         Free (Transform_Handle);
      end Free_Transform;
   begin
      --  Iterate over all attached facilities.
      Facilities_Stack_Package.Iterate (Container => Logger.F_Stack,
               Process   => Free_Facility'Access);
      Logger.F_Stack.Clear;

      --  Iterate over all attached transforms.
      Transforms_Stack_Package.Iterate (Container => Logger.T_Stack,
               Process   => Free_Transform'Access);
      Logger.T_Stack.Clear;
   end Finalize;

   -------------------------------------------------------------------------

   procedure Initialize (Logger : in out Instance) is
   begin
      if Logger.Init then
         declare
            Default_Handle : Facilities.File_Descriptor.Handle;
         begin
            Default_Handle := new Facilities.File_Descriptor.Instance;
            Default_Handle.Set_Name (Name => "__Init_Facility");

            Logger.Attach_Facility
              (Facility => Facilities.Handle (Default_Handle));
         end;
      end if;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Log_Message (Logger : Instance;
                          Level  : Log_Level;
                          Msg    : String)
   is
      F_Position : Facilities_Stack_Package.Cursor := Logger.F_Stack.First;
      T_Position : Transform_List_Package.Cursor;
      F_Item     : Alog.Facilities.Handle;
      T_Item     : Alog.Transforms.Handle;
      T_List     : Alog.Facilities.Transform_List_Package.List;
      Out_Msg    : String := Msg;
   begin
      --  Loop over all facilities.
      while Facilities_Stack_Package.Has_Element (F_Position) loop
         F_Item := Facilities_Stack_Package.Element (F_Position);

         --  Apply all transformations.
         if F_Item.Transform_Count > 0 then
            T_List := F_Item.Get_Transforms;
            T_Position := T_List.First;
            while Transform_List_Package.Has_Element (T_Position) loop
               T_Item := Transform_List_Package.Element (T_Position);
               Out_Msg := T_Item.Transform_Message (Level => Level,
                                                    Msg   => Out_Msg);
               Transform_List_Package.Next (T_Position);
            end loop;
         end if;
         F_Item.Write_Message (Level => Level,
                               Msg   => Out_Msg);
         Facilities_Stack_Package.Next (F_Position);
      end loop;
   end Log_Message;

   -------------------------------------------------------------------------

   function Transform_Count (Logger : Instance) return Natural is
   begin
      return Natural (Logger.T_Stack.Length);
   end Transform_Count;

end Alog.Logger;
