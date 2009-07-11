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

with Ada.Unchecked_Deallocation;

package body Alog.Controlled_Containers is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Ada.Exceptions.Exception_Occurrence,
      Name   => Ada.Exceptions.Exception_Occurrence_Access);
   --  Free memory allocated by an Exception_Occurrence.

   -------------------------------------------------------------------------

   procedure Clear (Map : in out Exception_Map) is

      procedure Do_Free (Position : Containers.MOEOP.Cursor);
      --  Free the memory of the element.

      procedure Do_Free (Position : Containers.MOEOP.Cursor) is
         Handle : Ada.Exceptions.Exception_Occurrence_Access :=
           Containers.MOEOP.Element (Position => Position);
      begin
         Free (X => Handle);
      end Do_Free;

   begin
      Map.Data.Iterate (Process => Do_Free'Access);
      Map.Data.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Contains
     (Map : Exception_Map;
      Key : Ada.Task_Identification.Task_Id)
      return Boolean
   is
   begin
      return Map.Data.Contains (Key => Key);
   end Contains;

   -------------------------------------------------------------------------

   procedure Delete (Map : in out Exception_Map;
                     Key :        Ada.Task_Identification.Task_Id) is
      Handle : Ada.Exceptions.Exception_Occurrence_Access :=
        Map.Data.Element (Key => Key);
   begin
      Free (X => Handle);
      Map.Data.Delete (Key => Key);
   end Delete;

   -------------------------------------------------------------------------

   procedure Finalize (Map : in out Exception_Map) is
   begin
      Map.Clear;
   end Finalize;

   -------------------------------------------------------------------------

   procedure Get
     (Map     : in out Exception_Map;
      Key     :        Ada.Task_Identification.Task_Id;
      Element :    out Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Ada.Exceptions.Save_Occurrence
        (Target => Element,
         Source => Map.Data.Element (Key => Key).all);
   end Get;

   -------------------------------------------------------------------------

   procedure Insert
     (Map  : in out Exception_Map;
      Key  :        Ada.Task_Identification.Task_Id;
      Item :        Ada.Exceptions.Exception_Occurrence_Access)
   is
   begin
      Map.Data.Insert (Key      => Key,
                       New_Item => Item);
   end Insert;

   -------------------------------------------------------------------------

   function Is_Empty (Map : Exception_Map) return Boolean is
   begin
      return Map.Data.Is_Empty;
   end Is_Empty;

end Alog.Controlled_Containers;
