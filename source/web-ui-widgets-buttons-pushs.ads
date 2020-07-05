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
-- Copyright © 2017-2020, Vadim Godunko <vgodunko@gmail.com>                --
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
--  $Revision: 5741 $ $Date: 2017-01-28 23:47:07 +0300 (Сб, 28 янв 2017) $
------------------------------------------------------------------------------

with Web.Core.Slots_0;
private with Web.Core.Slots_0.Emitters;
with Web.Strings;
with Web.HTML.Buttons;

package Web.UI.Widgets.Buttons.Pushs is

   type Push_Button is new Web.UI.Widgets.Buttons.Abstract_Button with private;

   type Push_Button_Access is access all Push_Button'Class
     with Storage_Size => 0;

   not overriding function Clicked_Signal
    (Self : in out Push_Button)
       return not null access Web.Core.Slots_0.Signal'Class;

   package Constructors is

      function Create
       (Element :in out Web.HTML.Buttons.HTML_Button_Element'Class)
            return not null Push_Button_Access;

      function Create
       (Id : Web.Strings.Web_String) return not null Push_Button_Access;

      procedure Initialize
       (Self    : in out Push_Button'Class;
        Element : in out Web.HTML.Buttons.HTML_Button_Element'Class);

   end Constructors;

private

   type Push_Button is
     new Web.UI.Widgets.Buttons.Abstract_Button with record
      Clicked : aliased Web.Core.Slots_0.Emitters.Emitter
                         (Push_Button'Unchecked_Access);
   end record;

   overriding procedure Mouse_Click_Event
    (Self  : in out Push_Button;
     Event : in out Web.UI.Events.Mouse.Mouse_Event'Class);

   overriding procedure Set_Disabled
    (Self     : in out Push_Button;
     Disabled : Boolean);

end Web.UI.Widgets.Buttons.Pushs;
