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
--  $Revision: 5743 $ $Date: 2017-01-29 12:06:05 +0300 (Sun, 29 Jan 2017) $
------------------------------------------------------------------------------
--  Two state version of the check box
------------------------------------------------------------------------------

with Web.HTML.Inputs;
with Web.Strings;

private with Web.UI.Boolean_Slots.Emitters;

package Web.UI.Widgets.Buttons.Boolean_Check_Boxes is

   type Boolean_Check_Box is
     new Web.UI.Widgets.Buttons.Abstract_Button with private;

   type Boolean_Check_Box_Access is access all Boolean_Check_Box'Class
     with Storage_Size => 0;

   function State_Changed_Signal
    (Self : in out Boolean_Check_Box'Class)
       return not null access Web.UI.Boolean_Slots.Signal'Class;

   package Constructors is

      function Create
       (Element : Web.HTML.Inputs.HTML_Input_Element'Class)
          return not null Boolean_Check_Box_Access;

      function Create
       (Id : Web.Strings.Web_String) return not null Boolean_Check_Box_Access;

      procedure Initialize
       (Self    : in out Boolean_Check_Box'Class;
        Element : Web.HTML.Inputs.HTML_Input_Element'Class);

   end Constructors;

private

   type Boolean_Check_Box is
     new Web.UI.Widgets.Buttons.Abstract_Button with record
      State_Changed : aliased
        Web.UI.Boolean_Slots.Emitters.Emitter
         (Boolean_Check_Box'Unchecked_Access);
   end record;

   overriding procedure Change_Event (Self : in out Boolean_Check_Box);

--   overriding procedure Set_Enabled
--    (Self    : in out Check_Box;
--     Enabled : Boolean);

end Web.UI.Widgets.Buttons.Boolean_Check_Boxes;
