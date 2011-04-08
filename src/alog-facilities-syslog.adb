--
--  Copyright (c) 2008-2011,
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

with Interfaces.C.Strings;

package body Alog.Facilities.Syslog is

   package C renames Interfaces.C;

   type L_Type is mod 2 ** 3;
   for L_Type'Size use 3;

   Level_Map : constant array (Log_Level) of L_Type
     := (Debug     => 7,
         Info      => 6,
         Notice    => 5,
         Warning   => 4,
         Error     => 3,
         Critical  => 2,
         Alert     => 1,
         Emergency => 0);

   -------------------------------------------------------------------------

   procedure Write
     (Facility : Instance;
      Level    : Log_Level := Info;
      Msg      : String)
   is
      pragma Unreferenced (Facility);

      procedure Syslog_Wrapper
        (Prio : C.int;
         Msg  : C.Strings.chars_ptr);
      pragma Import (C, Syslog_Wrapper, "syslog_wrapper");

      C_Msg : C.Strings.chars_ptr := C.Strings.New_String (Str => Msg);
   begin
      Syslog_Wrapper (Prio => C.int (Level_Map (Level)),
                      Msg  => C_Msg);

      C.Strings.Free (C_Msg);
   end Write;

end Alog.Facilities.Syslog;
