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
--  ComboBox widget for string values.
------------------------------------------------------------------------------

with Web.HTML.Selects;
with Web.Strings;

--with Web.Core.Connectables.Slots_0.Slots_1.Generic_Emitters;
--with Web.Core.Slots_1;
--with Core.Slots_0;
----private with Core.Slots_0.Generic_Slots;
----private with Core.Slots_0.Emitters;
--with WUI.String_Slots;
--private with WUI.String_Slots.Emitters;
private with Web.UI.String_Slots.Emitters;

package Web.UI.Widgets.Combo_Boxes.Strings is

   --  XXX Type of Index should be changed to Integer.

   type String_Combo_Box is
     new Web.UI.Widgets.Combo_Boxes.Abstract_Combo_Box with private;

   type String_Combo_Box_Access is access all String_Combo_Box'Class
     with Storage_Size => 0;

   function Current_Value
    (Self : in out String_Combo_Box'Class) return Web.Strings.Web_String;

--   not overriding procedure Set_Current_Index
--    (Self : in out Combo_Box;
--     To   : League.Strings.Universal_String);
--   --  Available as slot.

   -------------
   -- Signals --
   -------------

--   not overriding function Current_Index_Changed_Signal
--    (Self : in out Combo_Box)
--       return not null access WUI.String_Slots.Signal'Class;

   package Constructors is

      procedure Initialize
       (Self    : in out String_Combo_Box'Class;
        Element : Web.HTML.Selects.HTML_Select_Element'Class);

      function Create
       (Element : Web.HTML.Selects.HTML_Select_Element)
          return not null String_Combo_Box_Access;

      function Create
       (Id : Web.Strings.Web_String) return not null String_Combo_Box_Access;

   end Constructors;

private

   type String_Combo_Box is
     new Web.UI.Widgets.Combo_Boxes.Abstract_Combo_Box with record
      Current_Index_Changed : aliased
        Web.UI.String_Slots.Emitters.Emitter
         (String_Combo_Box'Unchecked_Access);
   end record;

--   overriding procedure Input_Event (Self : in out Combo_Box);

end Web.UI.Widgets.Combo_Boxes.Strings;
