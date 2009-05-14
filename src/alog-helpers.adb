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

package body Alog.Helpers is

   -------------------------------------------------------------------------

   function Assert_Files_Equal
     (Filename1 : String;
      Filename2 : String)
      return Boolean
   is
      File1        : D_IO.File_Type;
      File2        : D_IO.File_Type;
      Char1, Char2 : My_Rec;
      Result       : Boolean := True;
   begin
      --  Open both files.
      Open (File => File1,
            Mode => In_File,
            Name => Filename1);

      Open (File => File2,
            Mode => In_File,
            Name => Filename2);

      --  Check length of files first.
      if Size (File1) /= Size (File2) then
         Close (File => File1);
         Close (File => File2);
         return False;
      end if;

      while not End_Of_File (File1) loop
         --  Read one byte from both files.
         Read (File => File1, Item => Char1);
         Read (File => File2, Item => Char2);
         --  Compare it.
         if Char1 /= Char2 then
            Result := False;
         end if;
      end loop;

      --  Close them files again.
      Close (File => File1);
      Close (File => File2);

      return Result;

   end Assert_Files_Equal;

end Alog.Helpers;
