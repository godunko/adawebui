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
--  $Revision: 5726 $ $Date: 2017-01-26 00:26:30 +0300 (Thu, 26 Jan 2017) $
------------------------------------------------------------------------------

--with Core.Slots_0;
--private with Core.Slots_0.Emitters;

package Web.UI.Widgets.Buttons is

   type Abstract_Button is
     abstract new Web.UI.Widgets.Abstract_Widget with private;

--   not overriding function Clicked_Signal
--    (Self : in out Abstract_Button)
--       return not null access Core.Slots_0.Signal'Class;

   package Constructors is

      procedure Initialize
       (Self    : in out Abstract_Button'Class;
        Element : Web.HTML.Elements.HTML_Element'Class);

   end Constructors;

private

   type Abstract_Button is
     abstract new Web.UI.Widgets.Abstract_Widget with record
      null;
--      Clicked : aliased Core.Slots_0.Emitters.Emitter
--                         (Abstract_Button'Unchecked_Access);
   end record;

--   overriding procedure Click_Event
--    (Self  : in out Abstract_Button;
--     Event : in out WUI.Events.Mouse.Click.Click_Event'Class);

end Web.UI.Widgets.Buttons;
