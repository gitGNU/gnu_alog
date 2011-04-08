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

   -------------------------------------------------------------------------

   procedure Write
     (Facility : Instance;
      Level    : Log_Level := Info;
      Msg      : String)
   is
      pragma Unreferenced (Facility);

      procedure C_Syslog
        (Prio : C.int;
         Msg  : C.Strings.chars_ptr);
      pragma Import (C, C_Syslog, "syslog");

      Char_Ptr : C.Strings.chars_ptr;
   begin
      Char_Ptr := C.Strings.New_String (Str => Msg);
      C_Syslog (Prio => Log_Level'Pos (Level),
                Msg  => Char_Ptr);

      Interfaces.C.Strings.Free (Char_Ptr);
   end Write;

end Alog.Facilities.Syslog;
