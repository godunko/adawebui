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

with Web.Window;

package body Web.UI.Widgets.Buttons.Pushs is

   --------------------
   -- Clicked_Signal --
   --------------------

   not overriding function Clicked_Signal
     (Self : in out Push_Button)
      return not null access Core.Slots_0.Signal'Class is
   begin
      return Self.Clicked'Unchecked_Access;
   end Clicked_Signal;

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      type Push_Button_Internal_Access is access all Push_Button'Class;

      ------------
      -- Create --
      ------------

      function Create
       (Element : in out Web.HTML.Buttons.HTML_Button_Element'Class)
          return not null Push_Button_Access
      is
         Result : constant not null Push_Button_Internal_Access
           := new Push_Button;

      begin
         Initialize (Result.all, Element);

         return Push_Button_Access (Result);
      end Create;

      ------------
      -- Create --
      ------------

      function Create
       (Id : Web.Strings.Web_String) return not null Push_Button_Access
      is
         Element : Web.HTML.Buttons.HTML_Button_Element
           := Web.Window.Document.Get_Element_By_Id (Id).As_HTML_Button;

      begin
         return Create (Element);
      end Create;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
       (Self    : in out Push_Button'Class;
        Element : in out Web.HTML.Buttons.HTML_Button_Element'Class) is
      begin
         Web.UI.Widgets.Buttons.Constructors.Initialize (Self, Element);
      end Initialize;

   end Constructors;

   -----------------------
   -- Mouse_Click_Event --
   -----------------------

   overriding procedure Mouse_Click_Event
    (Self  : in out Push_Button;
     Event : in out Web.UI.Events.Mouse.Mouse_Event'Class) is
   begin
      --  XXX Why it is 'mouse click'? It seems it is called on press of Space
      --  too.

      Abstract_Button (Self).Mouse_Click_Event (Event);
--        Event.Accept_Event;
      Self.Clicked.Emit;
   end Mouse_Click_Event;

   ------------------
   -- Set_Disabled --
   ------------------

   overriding procedure Set_Disabled
    (Self     : in out Push_Button;
     Disabled : Boolean)
   is
      Element : Web.HTML.Buttons.HTML_Button_Element
        := Self.Element.As_HTML_Button;

   begin
      Element.Set_Disabled (Disabled);
   end Set_Disabled;

end Web.UI.Widgets.Buttons.Pushs;
