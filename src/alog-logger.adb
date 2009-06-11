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
      use Facilities_Stack_Package;

      F_Name : constant Unbounded_String :=
        To_Unbounded_String (Facility.Get_Name);
      Position : Cursor;
   begin
      Position := Logger.F_Stack.Find (Key => F_Name);

      if Position /= No_Element then
         raise Facility_Already_Present with "Facility '"
           & To_String (F_Name)
           & "' is already present.";
      end if;

      Logger.F_Stack.Insert
        (Key      =>  F_Name,
         New_Item => Facility);
   end Attach_Facility;

   -------------------------------------------------------------------------

   procedure Attach_Transform (Logger    : in out Instance;
                               Transform :        Alog.Transforms.Handle)
   is
      use Transforms_Stack_Package;

      T_Name : constant Unbounded_String :=
        To_Unbounded_String (Transform.Get_Name);
      Position : Cursor;
   begin
      Position := Logger.T_Stack.Find (Key => T_Name);

      if Position /= No_Element then
         raise Transform_Already_Present with "Transform '"
           & To_String (T_Name)
           & "' is already present.";
      end if;

      Logger.T_Stack.Insert
        (Key      =>  T_Name,
         New_Item => Transform);
   end Attach_Transform;

   -------------------------------------------------------------------------

   procedure Clear (L : in out Instance) is
   begin
      L.Finalize;
   end Clear;

   -------------------------------------------------------------------------

   procedure Detach_Facility (Logger : in out Instance;
                              Name   :        String)
   is
      use Facilities_Stack_Package;

      Position        : Cursor;
      Facility_Handle : Facilities.Handle;
      F_Name          : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      Position := Logger.F_Stack.Find (Key => F_Name);

      if Position = No_Element then
         raise Facility_Not_Found with "Facility '"
           & Name & "' not found.";
      end if;

      Facility_Handle := Element (Position);

      Logger.F_Stack.Delete (Key => F_Name);

      --  Free memory.
      Free (Facility_Handle);
   end Detach_Facility;

   -------------------------------------------------------------------------

   procedure Detach_Transform (Logger : in out Instance;
                               Name   :        String)
   is
      use Transforms_Stack_Package;

      Position : Cursor;
      T_Handle : Transforms.Handle;
      T_Name   : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      Position := Logger.T_Stack.Find (Key => T_Name);

      if Position = No_Element then
         raise Transform_Not_Found with "Transform '"
           & Name & "' not found.";
      end if;

      T_Handle := Element (Position);

      Logger.T_Stack.Delete (Key => T_Name);

      --  Free memory.
      Free (T_Handle);
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
         Facility_Handle : Facilities.Handle := Element (Position);
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
         Transform_Handle : Transforms.Handle := Element (Position);
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

   function Get_Facility
     (Logger : Instance;
      Name   : String)
      return Facilities.Handle
   is
      use Facilities_Stack_Package;

      Position : Cursor;
   begin
      Position := Logger.F_Stack.Find (Key => To_Unbounded_String (Name));

      if Position = No_Element then
         raise Facility_Not_Found with "Facility '" & Name & "' not found.";
      end if;

      return Element (Position => Position);
   end Get_Facility;

   -------------------------------------------------------------------------

   function Get_Transform
     (Logger : Instance;
      Name   : String)
      return Transforms.Handle
   is
      use Transforms_Stack_Package;

      Position : Cursor;
   begin
      Position := Logger.T_Stack.Find (Key => To_Unbounded_String (Name));

      if Position = No_Element then
         raise Transform_Not_Found with "Transform '" & Name & "' not found.";
      end if;

      return Element (Position => Position);
   end Get_Transform;

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
      F_Item     : Facilities.Handle;
      Out_Msg    : String := Msg;

      procedure Do_Transform (Transform : Transforms.Handle);
      --  Call 'Transform_Message' for each transform.

      procedure Do_Transform (Transform : Transforms.Handle) is
      begin
         Out_Msg := Transform.Transform_Message (Level => Level,
                                                 Msg   => Out_Msg);
      end Do_Transform;
   begin
      --  Loop over all facilities.
      while Facilities_Stack_Package.Has_Element (F_Position) loop
         F_Item := Facilities_Stack_Package.Element (F_Position);

         --  Apply all transformations.
         if F_Item.Transform_Count > 0 then
            F_Item.Iterate (Process => Do_Transform'Access);
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
