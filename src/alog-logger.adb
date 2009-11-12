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
   begin
      if not Logger.F_Stack.Contains
        (Key => To_Unbounded_String (Default_Facility_Name)) then
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
      F_Name : constant Unbounded_String :=
        To_Unbounded_String (Facility.Get_Name);
   begin
      if Logger.F_Stack.Contains (Key => F_Name) then
         raise Facility_Already_Present with "Facility '"
           & To_String (F_Name)
           & "' is already present.";
      end if;

      Logger.F_Stack.Insert
        (Key      => F_Name,
         New_Item => Facility);
   end Attach_Facility;

   -------------------------------------------------------------------------

   procedure Attach_Transform
     (Logger    : in out Instance;
      Transform :        Transforms.Handle)
   is
      T_Name : constant Unbounded_String :=
        To_Unbounded_String (Transform.Get_Name);
   begin
      if Logger.T_Stack.Contains (Key => T_Name) then
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

      procedure Teardown_Facility (Handle : Facilities.Handle);
      --  Teardown a facility.

      procedure Teardown_Facility (Handle : Facilities.Handle) is
      begin
         Handle.Teardown;
      end Teardown_Facility;

      procedure Teardown_Transform (Handle : Transforms.Handle);
      --  Teardown a transform.

      procedure Teardown_Transform (Handle : Transforms.Handle) is
      begin
         Handle.Teardown;
      end Teardown_Transform;

   begin
      L.F_Stack.Iterate (Process => Teardown_Facility'Access);
      L.F_Stack.Clear;

      L.T_Stack.Iterate (Process => Teardown_Transform'Access);
      L.T_Stack.Clear;
   end Clear;

   -------------------------------------------------------------------------

   procedure Detach_Default_Facility (Logger : in out Instance)
   is
   begin
      if Logger.F_Stack.Contains
        (Key => To_Unbounded_String (Default_Facility_Name)) then
         Logger.Detach_Facility (Name => Default_Facility_Name);
      end if;
   end Detach_Default_Facility;

   -------------------------------------------------------------------------

   procedure Detach_Facility
     (Logger : in out Instance;
      Name   :        String)
   is
      F_Name : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if not Logger.F_Stack.Contains (Key => F_Name) then
         raise Facility_Not_Found with "Facility '"
           & Name & "' not found.";
      end if;

      Logger.F_Stack.Delete (Key => F_Name);
   end Detach_Facility;

   -------------------------------------------------------------------------

   procedure Detach_Transform
     (Logger : in out Instance;
      Name   :        String)
   is
      T_Name   : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if not Logger.T_Stack.Contains (Key => T_Name) then
         raise Transform_Not_Found with "Transform '"
           & Name & "' not found.";
      end if;

      Logger.T_Stack.Delete (Key => T_Name);
   end Detach_Transform;

   -------------------------------------------------------------------------

   function Facility_Count (Logger : Instance) return Natural is
   begin
      return Natural (Logger.F_Stack.Length);
   end Facility_Count;

   -------------------------------------------------------------------------

   procedure Finalize (Logger : in out Instance) is
   begin
      Logger.Clear;
   end Finalize;

   -------------------------------------------------------------------------

   function Get_Source_Loglevel
     (Logger : Instance;
      Source : String)
      return Log_Level
   is
   begin
      return Logger.Sources.Element
        (Key => To_Unbounded_String (Source));

   exception
      when Constraint_Error =>
         raise No_Source_Loglevel with
           "No loglevel for source '" & Source & "'";
   end Get_Source_Loglevel;

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
        procedure (Facility_Handle : Facilities.Handle))
   is
   begin
      Logger.F_Stack.Iterate (Process => Process);
   end Iterate;

   -------------------------------------------------------------------------

   procedure Iterate
     (Logger  : Instance;
      Process : not null access procedure
        (Transform_Handle : Transforms.Handle))
   is
   begin
      Logger.T_Stack.Iterate (Process => Process);
   end Iterate;

   -------------------------------------------------------------------------

   procedure Log_Message
     (Logger : Instance;
      Level  : Log_Level;
      Msg    : String)
   is
      Out_Msg : String := Msg;

      procedure Do_Log (Facility_Handle : Facilities.Handle);
      --  Log message for each facility.

      procedure Do_Log (Facility_Handle : Facilities.Handle)
      is
      begin
         Facility_Handle.Log_Message (Level => Level,
                                      Msg   => Out_Msg);
      end Do_Log;

      procedure Do_Transform (Transform_Handle : Transforms.Handle);
      --  Call 'Transform_Message' for each transform.

      procedure Do_Transform (Transform_Handle : Transforms.Handle) is
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

   procedure Set_Source_Loglevel
     (Logger : in out Instance;
      Source :        String;
      Level  :        Log_Level)
   is
      use type MOSLP.Cursor;

      Key      : constant Unbounded_String := To_Unbounded_String (Source);
      Position : MOSLP.Cursor;
   begin
      Position := Logger.Sources.Find (Key => Key);

      if Position = MOSLP.No_Element then
         Logger.Sources.Insert
           (Key      => Key,
            New_Item => Level);
      else
         Logger.Sources.Replace_Element
           (Position => Position,
            New_Item => Level);
      end if;
   end Set_Source_Loglevel;

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
        procedure (Facility_Handle : Facilities.Handle))
   is
      F_Name : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if not Logger.F_Stack.Contains (Key => F_Name) then
         raise Facility_Not_Found with "Facility '" & Name & "' not found";
      end if;

      declare
         Handle : constant Facilities.Handle :=
           Logger.F_Stack.Element (Key => F_Name);
      begin
         Process (Facility_Handle => Handle);
      end;
   end Update;

   -------------------------------------------------------------------------

   procedure Update
     (Logger  : Instance;
      Name    : String;
      Process : not null access
        procedure (Transform_Handle : Transforms.Handle))
   is
      T_Name : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if not Logger.T_Stack.Contains (Key => T_Name) then
         raise Transform_Not_Found with "Transform '" & Name & "' not found";
      end if;

      declare
         Handle : constant Transforms.Handle :=
           Logger.T_Stack.Element (Key => T_Name);
      begin
         Process (Transform_Handle => Handle);
      end;
   end Update;

end Alog.Logger;
