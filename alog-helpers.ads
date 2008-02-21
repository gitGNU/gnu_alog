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

with Ada.Direct_IO;

--  Alog helper functions/procedures.
package Alog.Helpers is

   function Assert_Files_Equal (Filename1 : String;
                                Filename2 : String) return Boolean;
   --  Compare two files byte-wise. Returns True if both files are equal.
   --  The two files are closed but not removed after comparison.

private
   type MY_REC is
      record
         Char : Character;
      end record;

   package D_IO is new Ada.Direct_IO (MY_REC);
   use D_IO;

end Alog.Helpers;
