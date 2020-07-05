------------------------------------------------------------------------------
--                                                                          --
--                            Matreshka Project                             --
--                                                                          --
--                               Web Framework                              --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2016-2020, Vadim Godunko <vgodunko@gmail.com>                --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Vadim Godunko, IE nor the names of its        --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------
--  $Revision: 5703 $ $Date: 2017-01-20 22:17:20 +0300 (Пт, 20 янв 2017) $
------------------------------------------------------------------------------

package body Web.Core.Connectables.Slots_0.Emitters is

   -------------
   -- Connect --
   -------------

   overriding procedure Connect
    (Self : in out Emitter;
     Slot : Slots_0.Slot'Class)
   is
      Slot_End   : Slot_End_Access := Slot.Create_Slot_End;
      Signal_End : Signal_End_Access
        := new Emitters.Signal_End (Self'Unchecked_Access);

   begin
      Slot_End.Attach;
      Signal_End.Attach;
      Signal_End.Slot_End := Slot_End;
   end Connect;

   ----------
   -- Emit --
   ----------

   procedure Emit (Self : in out Emitter'Class) is
      Current : Signal_End_Access := Self.Head;

   begin
      while Current /= null loop
         begin
            Signal_End'Class (Current.all).Invoke;

         exception
            when others =>
               null;
         end;

         Current := Current.Next;
      end loop;
   end Emit;

   ------------
   -- Invoke --
   ------------

   procedure Invoke (Self : in out Signal_End'Class) is
   begin
      Slot_End_0'Class (Self.Slot_End.all).Invoke;
   end Invoke;

end Web.Core.Connectables.Slots_0.Emitters;
