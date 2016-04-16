--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Data_Bank.Indexed.                Luebeck            --
--        Text_IO                                  Summer, 2009       --
--  Implementation                                                    --
--                                Last revision :  09:26 07 Aug 2009  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

package body Persistent.Data_Bank.Indexed.Text_IO is
   use Storage_Index_IO;

   procedure Put
             (  File    : File_Type;
                Storage : in out Indexed_Storage_Object'Class
             )  is
   begin
      Storage.Lock.Seize;
      begin
         Put (File, Storage.Index);
      exception
         when others =>
            Storage.Lock.Release;
            raise;
      end;
      Storage.Lock.Release;
   end Put;

   procedure Put (Storage : in out Indexed_Storage_Object'Class) is
   begin
      Put (Storage.Index);
   end Put;

end Persistent.Data_Bank.Indexed.Text_IO;
