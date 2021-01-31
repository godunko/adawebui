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
-- Copyright © 2016-2021, Vadim Godunko <vgodunko@gmail.com>                --
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
--  $Revision: 5682 $ $Date: 2017-01-11 00:55:53 +0300 (Wed, 11 Jan 2017) $
------------------------------------------------------------------------------

package body Web.UI.Events.Mouse is

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
       (Self  : in out Mouse_Event'Class;
        Event : in out Web.UI_Events.Mouse_Events.Mouse_Event'Class) is
      begin
         Web.UI.Events.Constructors.Initialize (Self, Event);
      end Initialize;

   end Constructors;

   -------------
   -- Buttons --
   -------------

   function Buttons (Self : Mouse_Event'Class) return Mouse_Buttons is
      Buttons : constant Web.DOM_Unsigned_Short
        := Self.Event.As_Mouse_Event.Buttons;
      Result  : Mouse_Buttons := (others => False);

   begin
      if (Buttons and 2#0001#) /= 0 then
         Result (Button_1) := True;
      end if;

      if (Buttons and 2#0010#) /= 0 then
         Result (Button_2) := True;
      end if;

      if (Buttons and 2#0100#) /= 0 then
         Result (Button_3) := True;
      end if;

      return Result;
   end Buttons;

   -------
   -- X --
   -------

   function X (Self : Mouse_Event'Class) return Long_Float is
   begin
      return Long_Float (Self.Event.As_Mouse_Event.Offset_X);
   end X;

   -------
   -- Y --
   -------

   function Y (Self : Mouse_Event'Class) return Long_Float is
   begin
      return Long_Float (Self.Event.As_Mouse_Event.Offset_Y);
   end Y;

end Web.UI.Events.Mouse;
