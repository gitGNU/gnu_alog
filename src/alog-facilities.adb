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

with GNAT.Calendar.Time_IO;

package body Alog.Facilities is

   -------------------------------------------------------------------------

   function "=" (Left  : Handle;
                 Right : Handle) return Boolean is
   begin
      return Left.Get_Name = Right.Get_Name;
   end "=";

   -------------------------------------------------------------------------

   procedure Add_Transform (Facility  : in out Class;
                            Transform :        Transforms.Handle)
   is
      T_Name : constant Unbounded_String :=
        To_Unbounded_String (Transform.Get_Name);
   begin
      if Facility.Transforms.Contains (Key => T_Name) then
         raise Transform_Already_Present with "Transform '"
           & To_String (T_Name)
           & "' is already present.";
      end if;

      Facility.Transforms.Insert
        (Key      => T_Name,
         New_Item => Transform);
   end Add_Transform;

   -------------------------------------------------------------------------

   function Get_Name (Facility : Class) return String is
   begin
      return To_String (Facility.Name);
   end Get_Name;

   -------------------------------------------------------------------------

   function Get_Threshold (Facility : Class) return Log_Level is
   begin
      return Facility.Threshold;
   end Get_Threshold;

   -------------------------------------------------------------------------

   function Get_Timestamp
     (Facility : Class;
      Time     : Ada.Calendar.Time := Ada.Calendar.Clock)
      return String
   is
      use GNAT.Calendar.Time_IO;
      Timestamp : constant String := Image
        (Date    => Time,
         Picture => Picture_String (Facility.Timestamp_Format));
   begin
      return Timestamp;
   end Get_Timestamp;

   -------------------------------------------------------------------------

   function Is_UTC_Timestamp (Facility : Class) return Boolean is
   begin
      return Facility.UTC_Timestamp;
   end Is_UTC_Timestamp;

   -------------------------------------------------------------------------

   function Is_Write_Loglevel (Facility : Class) return Boolean is
   begin
      return Facility.Write_Loglevel;
   end Is_Write_Loglevel;

   -------------------------------------------------------------------------

   function Is_Write_Timestamp (Facility : Class) return Boolean is
   begin
      return Facility.Write_Timestamp;
   end Is_Write_Timestamp;

   -------------------------------------------------------------------------

   procedure Iterate
     (Facility : Class;
      Process  : not null access procedure (Transform : Transforms.Handle))
   is
   begin
      Facility.Transforms.Iterate (Process => Process);
   end Iterate;

   -------------------------------------------------------------------------

   procedure Log_Message (Facility : Class;
                          Level    : Log_Level := INFO;
                          Msg      : String)
   is
      Message : Unbounded_String;

      procedure Do_Transform (Transform : Transforms.Handle);
      --  Call 'Transform_Message' for each transform.

      procedure Do_Transform (Transform : Transforms.Handle) is
      begin
         Message := To_Unbounded_String
           (Transform.Transform_Message
              (Level => Level,
               Msg   => To_String (Message)));
      end Do_Transform;

   begin
      if Level <= Facility.Get_Threshold then
         if Facility.Is_Write_Timestamp then
            Append (Source   => Message,
                    New_Item => Facility.Get_Timestamp & " ");
         end if;
         if Facility.Is_Write_Loglevel then
            Append (Source   => Message,
                    New_Item => "[" & Log_Level'Image (Level) & "] ");
         end if;

         Append (Source   => Message,
                 New_Item => Msg);

         if Facility.Transform_Count > 0 then
            Facility.Iterate (Process => Do_Transform'Access);
         end if;

         Facility.Write_Message (Level => Level,
                                 Msg   => To_String (Message));
      end if;
   end Log_Message;

   -------------------------------------------------------------------------

   procedure Remove_Transform (Facility : in out Class;
                               Name     :        String)
   is
      T_Name : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if not Facility.Transforms.Contains (Key => T_Name) then
         raise Transform_Not_Found with "Transform '" & Name & "' not found.";
      end if;

      Facility.Transforms.Delete (Key => T_Name);
   end Remove_Transform;

   -------------------------------------------------------------------------

   procedure Set_Name (Facility : in out Class;
                       Name     :        String) is
   begin
      Facility.Name := To_Unbounded_String (Name);
   end Set_Name;

   -------------------------------------------------------------------------

   procedure Set_Threshold (Facility : in out Class;
                            Level    :        Log_Level) is
   begin
      Facility.Threshold := Level;
   end Set_Threshold;

   -------------------------------------------------------------------------

   procedure Toggle_UTC_Timestamp
     (Facility : in out Class;
      State    :        Boolean)
   is
   begin
      Facility.UTC_Timestamp := State;
   end Toggle_UTC_Timestamp;

   -------------------------------------------------------------------------

   procedure Toggle_Write_Loglevel (Facility : in out Class;
                                    State    :        Boolean)
   is
   begin
      Facility.Write_Loglevel := State;
   end Toggle_Write_Loglevel;

   -------------------------------------------------------------------------

   procedure Toggle_Write_Timestamp (Facility : in out Class;
                                     State    :        Boolean)
   is
   begin
      Facility.Write_Timestamp := State;
   end Toggle_Write_Timestamp;

   -------------------------------------------------------------------------

   function Transform_Count (Facility : Class) return Natural is
   begin
      return Natural (Facility.Transforms.Length);
   end Transform_Count;

   -------------------------------------------------------------------------

   procedure Update
     (Facility : Class;
      Name     : String;
      Process  : not null access
        procedure (Transform_Handle : in out Transforms.Handle))
   is
      Unbounded_Name : constant Unbounded_String :=
        To_Unbounded_String (Name);
   begin
      if not Facility.Transforms.Contains (Key => Unbounded_Name) then
         raise Transform_Not_Found with "Transform '" & Name & "' not found";
      end if;

      declare
         Handle : Transforms.Handle :=
           Facility.Transforms.Element (Key => Unbounded_Name);
      begin
         Process (Transform_Handle => Handle);
      end;
   end Update;

end Alog.Facilities;
