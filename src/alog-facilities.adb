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

package body Alog.Facilities is

   ------------
   -- Equals --
   ------------

   function "=" (Left  : Handle;
                 Right : Handle) return Boolean is
   begin
      return Left.Get_Name = Right.Get_Name;
   end "=";

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Facility : in out Instance'Class; Name : in String) is
   begin
      Facility.Name := To_Unbounded_String (Name);
   end Set_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Facility : in Instance'Class) return String is
   begin
      return To_String (Facility.Name);
   end Get_Name;

   -----------
   --  Hash --
   -----------

   function Hash (Element : Alog.Facilities.Handle)
                           return Ada.Containers.Hash_Type is
      use Ada.Strings.Unbounded;
   begin
      return Ada.Strings.Unbounded.Hash
        (Key => To_Unbounded_String (Element.Get_Name));
   end Hash;

   -------------------
   -- Set_Threshold --
   -------------------

   procedure Set_Threshold (Facility : in out Instance'Class;
                            Level    : in Log_Level) is
   begin
      Facility.Threshold := Level;
   end Set_Threshold;

   -------------------
   -- Get_Threshold --
   -------------------

   function Get_Threshold (Facility : in Instance'Class) return Log_Level is
   begin
      return Facility.Threshold;
   end Get_Threshold;

   ----------------------
   -- Create_Timestamp --
   ----------------------

   function Get_Timestamp (Facility : in Instance'Class) return String is
      use GNAT.Calendar.Time_IO;
      Timestamp : String := Image
        (Date    => Calendar.Clock,
         Picture => Picture_String (Facility.Timestamp_Format));
   begin
      return Timestamp;
   end Get_Timestamp;

   -------------
   -- Get_Pid --
   -------------

   function Get_Pid (Facility : in Instance'Class) return Integer is
      use System.OS_Interface;

      function pid_t_To_Integer is
        new Ada.Unchecked_Conversion (Source => pid_t,
                                      Target => Integer);
   begin
      return pid_t_To_Integer (System.OS_Interface.getpid);
   end Get_Pid;

   -------------------
   -- Add_Transform --
   -------------------

   procedure Add_Transform (Facility  : in out Instance'Class;
                            Transform : in     Alog.Transforms.Handle) is
   begin
      Facility.Transforms.Append (Transform);
   end Add_Transform;

   ----------------------
   -- Remove_Transform --
   ----------------------

   procedure Remove_Transform (Facility  : in out Instance'Class;
                               Transform : in     Alog.Transforms.Handle) is
      use Transform_List_Package;
      Position : Cursor;
   begin
      Position := Facility.Transforms.Find (Transform);
      if Position /= No_Element then
         Facility.Transforms.Delete (Position);
      end if;
   end Remove_Transform;

   ---------------------
   -- Transform_Count --
   ---------------------

   function Transform_Count (Facility : in Instance'Class)
                             return  Ada.Containers.Count_Type is
   begin
      return Facility.Transforms.Length;
   end Transform_Count;

   --------------------
   -- Get_Transforms --
   --------------------

   function Get_Transforms (Facility : in Instance'Class)
                            return Transform_List_Package.List is
   begin
      return Facility.Transforms;
   end Get_Transforms;

end Alog.Facilities;
