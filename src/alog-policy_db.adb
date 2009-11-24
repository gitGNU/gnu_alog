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

package body Alog.Policy_DB is

   Current_Default_Loglevel : Log_Level := Alog_Default_Level;
   --  Current default loglevel.

   Src_Levels : Maps.Wildcard_Level_Map;
   --  Source loglevels.

   -------------------------------------------------------------------------

   function Accept_Src
     (Source : String := "";
      Level  : Log_Level)
      return Boolean
   is
      use type Alog.Maps.Cursor;
      Position : Maps.Cursor;
   begin
      if Source'Length > 0 then
         Position := Src_Levels.Lookup (Key => Source);

         if Position /= Maps.No_Element
           and then Level < Maps.Element (Position => Position) then
            return False;
         end if;
      end if;

      if Level < Current_Default_Loglevel then
         return False;
      end if;

      return True;
   end Accept_Src;

   -------------------------------------------------------------------------

   function Get_Default_Loglevel return Log_Level is
   begin
      return Current_Default_Loglevel;
   end Get_Default_Loglevel;

   -------------------------------------------------------------------------

   function Get_Src_Loglevel (Source : String) return Log_Level is
   begin
      return Src_Levels.Element (Key => Source);

   exception
      when Constraint_Error =>
         raise No_Source_Loglevel with
           "No loglevel for source '" & Source & "'";
   end Get_Src_Loglevel;

   -------------------------------------------------------------------------

   procedure Reset is
   begin
      Current_Default_Loglevel := Alog_Default_Level;
      Src_Levels.Clear;
   end Reset;

   -------------------------------------------------------------------------

   procedure Set_Default_Loglevel (Level : Log_Level) is
   begin
      Current_Default_Loglevel := Level;
   end Set_Default_Loglevel;

   -------------------------------------------------------------------------

   procedure Set_Src_Loglevel
     (Source : String;
      Level  : Log_Level)
   is
   begin
      Src_Levels.Insert (Key  => Source,
                         Item => Level);
   end Set_Src_Loglevel;

   -------------------------------------------------------------------------

   procedure Set_Src_Loglevel (Sources : Maps.Wildcard_Level_Map) is
   begin
      Src_Levels := Sources;
   end Set_Src_Loglevel;

end Alog.Policy_DB;