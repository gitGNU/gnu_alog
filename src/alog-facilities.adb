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

   procedure Set_Name (F : in out Instance'Class; Name : in String) is
   begin
      F.Name := To_Unbounded_String (Name);
   end Set_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (F : in Instance'Class) return Unbounded_String is
   begin
      return F.Name;
   end Get_Name;

   function Get_Name (F : in Instance'Class) return String is
   begin
      return To_String (F.Name);
   end Get_Name;

   -------------------
   -- Set_Threshold --
   -------------------

   procedure Set_Threshold (F     : in out Instance'Class;
                            Level : in Log_Level) is
   begin
      F.Threshold := Level;
   end Set_Threshold;

   -------------------
   -- Get_Threshold --
   -------------------

   function Get_Threshold (F : in Instance'Class) return Log_Level is
   begin
      return F.Threshold;
   end Get_Threshold;

end Alog.Facilities;
