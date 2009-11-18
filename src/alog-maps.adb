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

package body Alog.Maps is

   -------------------------------------------------------------------------

   function Element
     (Map : Wildcard_Level_Map;
      Key : String)
      return Log_Level
   is
   begin
      return Map.Data.Element (Key => To_Unbounded_String (Key));
   end Element;

   -------------------------------------------------------------------------

   function Element (Position : Cursor) return Log_Level is
   begin
      return MOLP.Element (Position => MOLP.Cursor (Position));
   end Element;

   -------------------------------------------------------------------------

   function Find
     (Map : Wildcard_Level_Map;
      Key : String)
      return Cursor
   is
   begin
      return Cursor (Map.Data.Find (Key => To_Unbounded_String (Key)));
   end Find;

   -------------------------------------------------------------------------

   procedure Insert
     (Map  : in out Wildcard_Level_Map;
      Key  :        String;
      Item :        Log_Level)
   is
      Position  : MOLP.Cursor;
      Insert_Ok : Boolean := False;
   begin
      Map.Data.Insert (Key      => To_Unbounded_String (Key),
                       New_Item => Item,
                       Position => Position,
                       Inserted => Insert_Ok);

      if not Insert_Ok then
         Map.Data.Replace_Element (Position => Position,
                                   New_Item => Item);
      end if;
   end Insert;

end Alog.Maps;
