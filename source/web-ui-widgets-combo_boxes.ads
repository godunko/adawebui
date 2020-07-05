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
--  $Revision: 5761 $ $Date: 2017-05-20 11:06:31 +0300 (Sat, 20 May 2017) $
------------------------------------------------------------------------------

with Web.HTML.Selects;

----with Core.Slots_0;
----private with Core.Slots_0.Generic_Slots;
----private with Core.Slots_0.Emitters;
--with WUI.String_Slots;
private with Web.UI.Integer_Slots.Emitters;
--private with WUI.String_Slots.Emitters;

package Web.UI.Widgets.Combo_Boxes is

   --  XXX Type of Index should be changed to Integer.

   type Abstract_Combo_Box is
     abstract new Web.UI.Widgets.Abstract_Widget with private;

   type Combo_Box_Access is access all Abstract_Combo_Box'Class
     with Storage_Size => 0;

   overriding procedure Set_Disabled
    (Self     : in out Abstract_Combo_Box;
     Disabled : Boolean := True);
   --  Available as slot.

--   function Current_Index
--    (Self : in out Combo_Box'Class)
--       return League.Strings.Universal_String;
--
--   not overriding procedure Set_Current_Index
--    (Self : in out Combo_Box;
--     To   : League.Strings.Universal_String);
--   --  Available as slot.

   -------------
   -- Signals --
   -------------

   not overriding function Current_Index_Changed_Signal
    (Self : in out Abstract_Combo_Box)
       return not null access Web.UI.Integer_Slots.Signal'Class;

   package Constructors is

      procedure Initialize
       (Self    : in out Abstract_Combo_Box'Class;
        Element : Web.HTML.Selects.HTML_Select_Element'Class);

--      function Create
--       (Element :
--          not null WebAPI.HTML.Select_Elements.HTML_Select_Element_Access)
--            return not null Combo_Box_Access;
--
--      function Create
--       (Id : League.Strings.Universal_String)
--          return not null Combo_Box_Access;

   end Constructors;

private

   type Abstract_Combo_Box is new Web.UI.Widgets.Abstract_Widget with record
      Current_Index_Changed : aliased
        Web.UI.Integer_Slots.Emitters.Emitter
         (Abstract_Combo_Box'Unchecked_Access);
   end record;

   overriding procedure Change_Event (Self : in out Abstract_Combo_Box);

--   overriding procedure Input_Event (Self : in out Combo_Box);

end Web.UI.Widgets.Combo_Boxes;
