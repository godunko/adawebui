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

with Web.Window;

with Web.Core.Connectables;

package body Web.UI.Widgets.Buttons.Boolean_Check_Boxes is

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ------------
      -- Create --
      ------------

      function Create
       (Element : Web.HTML.Inputs.HTML_Input_Element'Class)
          return not null Boolean_Check_Box_Access
      is
         Aux : constant not null Web.Core.Connectables.Object_Access
           := new Boolean_Check_Box;

      begin
         return Result : constant not null Boolean_Check_Box_Access
           := Boolean_Check_Box_Access (Aux)
         do
            Initialize (Result.all, Element);
         end return;
      end Create;

      ------------
      -- Create --
      ------------

      function Create
       (Id : Web.Strings.Web_String)
          return not null Boolean_Check_Box_Access is
      begin
         return
           Create (Web.Window.Document.Get_Element_By_Id (Id).As_HTML_Input);
      end Create;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
       (Self    : in out Boolean_Check_Box'Class;
        Element : Web.HTML.Inputs.HTML_Input_Element'Class) is
      begin
         Web.UI.Widgets.Buttons.Constructors.Initialize (Self, Element);
      end Initialize;

   end Constructors;

   ------------------
   -- Change_Event --
   ------------------

   overriding procedure Change_Event (Self : in out Boolean_Check_Box) is
   begin
      Self.State_Changed.Emit (Self.Element.As_HTML_Input.Get_Checked);
   end Change_Event;

   ---------------------
   -- Get_Check_State --
   ---------------------

   function Get_Check_State
    (Self : Boolean_Check_Box'Class) return Boolean is
   begin
      return Self.Element.As_HTML_Input.Get_Checked;
   end Get_Check_State;

   ------------------
   -- Set_Disabled --
   ------------------

   overriding procedure Set_Disabled
    (Self     : in out Boolean_Check_Box;
     Disabled : Boolean := True)
   is
      Element : Web.HTML.Inputs.HTML_Input_Element
        := Self.Element.As_HTML_Input;

   begin
      Element.Set_Disabled (Disabled);
   end Set_Disabled;

   --------------------------
   -- State_Changed_Signal --
   --------------------------

   function State_Changed_Signal
    (Self : in out Boolean_Check_Box'Class)
       return not null access Web.UI.Boolean_Slots.Signal'Class is
   begin
      return Self.State_Changed'Unchecked_Access;
   end State_Changed_Signal;

end Web.UI.Widgets.Buttons.Boolean_Check_Boxes;
