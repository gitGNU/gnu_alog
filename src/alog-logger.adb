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

   procedure Attach_Default_Facility (Logger : in out Instance)
   is
      use Facilities_Stack_Package;

      Position : Cursor;
   begin
      Position := Logger.F_Stack.Find
        (Key => To_Unbounded_String (Default_Facility_Name));

      if Position = No_Element then
         declare
            Default_Handle : Facilities.File_Descriptor.Handle;
         begin
            Default_Handle := new Facilities.File_Descriptor.Instance;
            Default_Handle.Set_Name (Name => Default_Facility_Name);

            Logger.Attach_Facility
              (Facility => Facilities.Handle (Default_Handle));
         end;
      end if;
   end Attach_Default_Facility;

   -------------------------------------------------------------------------

   procedure Attach_Facility
     (Logger   : in out Instance;
      Facility :        Facilities.Handle)
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

   procedure Attach_Transform
     (Logger    : in out Instance;
      Transform :        Transforms.Handle)
   is
      use Containers.MOTP;

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

   procedure Detach_Default_Facility (Logger : in out Instance)
   is
      use Facilities_Stack_Package;

      Position : Cursor;
   begin
      Position := Logger.F_Stack.Find
        (Key => To_Unbounded_String (Default_Facility_Name));

      if Position /= No_Element then
         Logger.Detach_Facility (Name => Default_Facility_Name);
      end if;
   end Detach_Default_Facility;

   -------------------------------------------------------------------------

   procedure Detach_Facility
     (Logger : in out Instance;
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

   procedure Detach_Transform
     (Logger : in out Instance;
      Name   :        String)
   is
      use Containers.MOTP;

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
      procedure Free_Facility (F_Handle : in out Facilities.Handle);

      procedure Free_Facility (F_Handle : in out Facilities.Handle)
      is
      begin
         --  Cleanup this facility.
         F_Handle.Teardown;
         Free (F_Handle);
      end Free_Facility;

      --  Forward specs.
      procedure Free_Transform (Transform : in out Transforms.Handle);

      procedure Free_Transform (Transform : in out Transforms.Handle)
      is
      begin
         --  Cleanup this transform.
         Transform.Teardown;
         Free (Transform);
      end Free_Transform;
   begin
      --  Iterate over all attached facilities.
      Logger.Iterate (Process => Free_Facility'Access);
      Logger.F_Stack.Clear;

      --  Iterate over all attached transforms.
      Logger.Iterate (Process => Free_Transform'Access);
      Logger.T_Stack.Clear;
   end Finalize;

   -------------------------------------------------------------------------

   procedure Initialize (Logger : in out Instance) is
   begin
      if Logger.Init then
         Logger.Attach_Default_Facility;
      end if;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Iterate
     (Logger  : Instance;
      Process : not null access
        procedure (Facility_Handle : in out Facilities.Handle))
   is

      procedure Do_Process (Position : Facilities_Stack_Package.Cursor);
      --  Call 'Process' for each facility.

      procedure Do_Process (Position : Facilities_Stack_Package.Cursor) is
         F_Handle : Facilities.Handle;
      begin
         F_Handle := Facilities_Stack_Package.Element (Position => Position);

         Process (Facility_Handle => F_Handle);
      end Do_Process;

   begin
      Logger.F_Stack.Iterate (Process => Do_Process'Access);
   end Iterate;

   -------------------------------------------------------------------------

   procedure Iterate
     (Logger  : Instance;
      Process : not null access procedure
        (Transform_Handle : in out Transforms.Handle))
   is
      procedure Do_Process (Position : Containers.MOTP.Cursor);
      --  Call 'Process' for each Transform.

      procedure Do_Process (Position : Containers.MOTP.Cursor) is
         T_Handle : Transforms.Handle;
      begin
         T_Handle := Containers.MOTP.Element (Position => Position);

         Process (Transform_Handle => T_Handle);
      end Do_Process;

   begin
      Logger.T_Stack.Iterate (Process => Do_Process'Access);
   end Iterate;

   -------------------------------------------------------------------------

   procedure Log_Message
     (Logger : Instance;
      Level  : Log_Level;
      Msg    : String)
   is
      Out_Msg    : String := Msg;

      procedure Do_Log (Facility_Handle : in out Facilities.Handle);
      --  Log message for each facility.

      procedure Do_Log (Facility_Handle : in out Facilities.Handle)
      is
      begin
         Facility_Handle.Log_Message (Level => Level,
                                      Msg   => Out_Msg);
      end Do_Log;

      procedure Do_Transform (Transform_Handle : in out Transforms.Handle);
      --  Call 'Transform_Message' for each transform.

      procedure Do_Transform (Transform_Handle : in out Transforms.Handle) is
      begin
         Out_Msg := Transform_Handle.Transform_Message
           (Level => Level,
            Msg   => Out_Msg);
      end Do_Transform;

   begin
      Logger.Iterate (Process => Do_Transform'Access);
      Logger.Iterate (Process => Do_Log'Access);
   end Log_Message;

   -------------------------------------------------------------------------

   function Transform_Count (Logger : Instance) return Natural is
   begin
      return Natural (Logger.T_Stack.Length);
   end Transform_Count;

   -------------------------------------------------------------------------

   procedure Update
     (Logger  : Instance;
      Name    : String;
      Process : not null access
        procedure (Facility_Handle : in out Facilities.Handle))
   is
      use Facilities_Stack_Package;

      Position       : Cursor;
      Unbounded_Name : constant Unbounded_String :=
        To_Unbounded_String (Name);
   begin
      Position := Logger.F_Stack.Find
        (Key => Unbounded_Name);

      if Position = No_Element then
         raise Facility_Not_Found with "Facility '" & Name & "' not found";
      end if;

      declare
         Handle : Facilities.Handle :=
           Logger.F_Stack.Element (Key => Unbounded_Name);
      begin
         Process (Facility_Handle => Handle);
      end;
   end Update;

   -------------------------------------------------------------------------

   procedure Update
     (Logger  : Instance;
      Name    : String;
      Process : not null access
        procedure (Transform_Handle : in out Transforms.Handle))
   is
      use Containers.MOTP;

      Position       : Cursor;
      Unbounded_Name : constant Unbounded_String :=
        To_Unbounded_String (Name);
   begin
      Position := Logger.T_Stack.Find
        (Key => Unbounded_Name);

      if Position = No_Element then
         raise Transform_Not_Found with "Transform '" & Name & "' not found";
      end if;

      declare
         Handle : Transforms.Handle :=
           Logger.T_Stack.Element (Key => Unbounded_Name);
      begin
         Process (Transform_Handle => Handle);
      end;
   end Update;

end Alog.Logger;
