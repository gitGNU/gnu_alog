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

with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

package Alog.Maps is

   type Wildcard_Level_Map is tagged private;
   --  A map of loglevels with string as key type.

   type Cursor is private;
   --  Index for a map element.

   No_Element : constant Cursor;

   function Element
     (Map : Wildcard_Level_Map;
      Key : String)
      return Log_Level;
   --  Returns the loglevel element for given key.

   function Element (Position : Cursor) return Log_Level;
   --  Returns the loglevel element at given position.

   function Find
     (Map : Wildcard_Level_Map;
      Key : String)
      return Cursor;
   --  Returns the position for an element with specified key. If no element is
   --  found No_Element is returned.

   procedure Insert
     (Map  : in out Wildcard_Level_Map;
      Key  :        String;
      Item :        Log_Level);
   --  Insert given key/item pair into map. If given key is already present the
   --  associated item is replaced.

private

   use Ada.Strings.Unbounded;

   package Map_Of_Loglevels_Package is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Log_Level,
      Hash            => Hash,
      Equivalent_Keys => "=");

   package MOLP renames Map_Of_Loglevels_Package;

   type Wildcard_Level_Map is tagged record
      Data : MOLP.Map;
   end record;

   type Cursor is new MOLP.Cursor;

   No_Element : constant Cursor := Cursor (MOLP.No_Element);

end Alog.Maps;
