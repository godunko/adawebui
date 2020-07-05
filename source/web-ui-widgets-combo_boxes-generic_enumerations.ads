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
--  Generic ComboBox widget around enumeration type.
------------------------------------------------------------------------------

with Web.HTML.Selects;
with Web.Strings;

with Web.Core.Connectables.Slots_0.Slots_1.Generic_Emitters;
with Web.Core.Slots_1;
--with Core.Slots_0;
----private with Core.Slots_0.Generic_Slots;
----private with Core.Slots_0.Emitters;
--with WUI.String_Slots;
--private with WUI.String_Slots.Emitters;

generic
   type Data_Type is (<>);

   with package Data_Slots is new Web.Core.Slots_1 (Data_Type);

   with package Data_Slots_Emitters is new Data_Slots.Generic_Emitters;

package Web.UI.Widgets.Combo_Boxes.Generic_Enumerations is

   --  XXX Type of Index should be changed to Integer.

   type Combo_Box is
     new Web.UI.Widgets.Combo_Boxes.Abstract_Combo_Box with private;

   type Combo_Box_Access is access all Combo_Box'Class
     with Storage_Size => 0;

   function Get_Current_Value (Self : in out Combo_Box'Class) return Data_Type;

--   not overriding procedure Set_Current_Index
--    (Self : in out Combo_Box;
--     To   : League.Strings.Universal_String);
--   --  Available as slot.

   -------------
   -- Signals --
   -------------

   not overriding function Current_Value_Changed_Signal
    (Self : in out Combo_Box)
      return not null access Data_Slots.Signal'Class;

   package Constructors is

      function Create
       (Element : Web.HTML.Selects.HTML_Select_Element'Class)
          return not null Combo_Box_Access;

      function Create
       (Id : Web.Strings.Web_String) return not null Combo_Box_Access;

      procedure Initialize
       (Self    : in out Combo_Box'Class;
        Element : Web.HTML.Selects.HTML_Select_Element'Class);

   end Constructors;

private

   type Combo_Box is
     new Web.UI.Widgets.Combo_Boxes.Abstract_Combo_Box with record
      Current_Value_Changed : aliased
        Data_Slots_Emitters.Emitter (Combo_Box'Unchecked_Access);
   end record;

   overriding procedure Change_Event (Self : in out Combo_Box);

--   overriding procedure Input_Event (Self : in out Combo_Box);

end Web.UI.Widgets.Combo_Boxes.Generic_Enumerations;
