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
--  $Revision: 5872 $ $Date: 2018-09-22 11:56:07 +0200 (Sat, 22 Sep 2018) $
------------------------------------------------------------------------------

with Web.DOM.Event_Targets;
with Web.UI_Events.Mouse_Events;
--with WebAPI.UI_Events.Wheel;

with Web.UI.Applications.Internals;

package body Web.UI.Widgets is

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
       (Self    : aliased in out Abstract_Widget'Class;
        Element : Web.HTML.Elements.HTML_Element'Class) is
      begin
         Self.Element := Web.HTML.Elements.HTML_Element (Element);

         --  Connect event dispatchers.

         Self.Element.Add_Event_Listener
          (+"blur", Self.Blur'Unchecked_Access, False);
         Self.Element.Add_Event_Listener
          (+"change", Self.Change'Unchecked_Access, False);
         Self.Element.Add_Event_Listener
          (+"click", Self.Click'Unchecked_Access, False);
         Self.Element.Add_Event_Listener
          (+"focus", Self.Focus'Unchecked_Access, False);
--         WebAPI.DOM.Event_Targets.Add_Event_Listener
--          (Element, +"input", Self.Input'Access, False);
         Self.Element.Add_Event_Listener
          (+"mousemove", Self.Mouse_Move'Unchecked_Access, False);
         Self.Element.Add_Event_Listener
          (+"mousedown", Self.Mouse_Down'Unchecked_Access, False);
         Self.Element.Add_Event_Listener
          (+"mouseup", Self.Mouse_Up'Unchecked_Access, False);
         Self.Element.Add_Event_Listener
          (+"mouseenter", Self.Mouse_Enter'Unchecked_Access, False);
         Self.Element.Add_Event_Listener
          (+"mouseleave", Self.Mouse_Leave'Unchecked_Access, False);
--         WebAPI.DOM.Event_Targets.Add_Event_Listener
--          (Element, +"wheel", Self.Wheel'Access, False);
      end Initialize;

   end Constructors;

   --------------------
   -- Focus_In_Event --
   --------------------

   not overriding procedure Focus_In_Event (Self : in out Abstract_Widget) is
   begin
      Web.UI.Applications.Internals.Focus_In (Self'Unchecked_Access);
   end Focus_In_Event;

   ---------------------
   -- Focus_Out_Event --
   ---------------------

   not overriding procedure Focus_Out_Event (Self : in out Abstract_Widget) is
   begin
      Web.UI.Applications.Internals.Focus_Out (Self'Unchecked_Access);
   end Focus_Out_Event;

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event
    (Self  : in out Blur_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class) is
   begin
      Self.Owner.Focus_Out_Event;
   end Handle_Event;

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event
    (Self  : in out Change_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class) is
   begin
      Self.Owner.Change_Event;
   end Handle_Event;

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event
    (Self  : in out Focus_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class) is
   begin
      Self.Owner.Focus_In_Event;
   end Handle_Event;

--   ------------------
--   -- Handle_Event --
--   ------------------
--
--   overriding procedure Handle_Event
--    (Self  : not null access Input_Dispatcher;
--     Event : access WebAPI.DOM.Events.Event'Class) is
--   begin
--      Self.Owner.Input_Event;
--   end Handle_Event;

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event
    (Self  : in out Mouse_Click_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class)
   is
      DOM_Event : Web.UI_Events.Mouse_Events.Mouse_Event
        := Event.As_Mouse_Event;
      UI_Event  : Web.UI.Events.Mouse.Mouse_Event;

   begin
      Web.UI.Events.Mouse.Constructors.Initialize (UI_Event, DOM_Event);
      Self.Owner.Mouse_Click_Event (UI_Event);
   end Handle_Event;

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event
    (Self  : in out Mouse_Move_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class)
   is
      DOM_Event : Web.UI_Events.Mouse_Events.Mouse_Event
        := Event.As_Mouse_Event;
      UI_Event  : Web.UI.Events.Mouse.Mouse_Event;

   begin
      Web.UI.Events.Mouse.Constructors.Initialize (UI_Event, DOM_Event);
      Self.Owner.Mouse_Move_Event (UI_Event);
   end Handle_Event;

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event
    (Self  : in out Mouse_Down_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class)
   is
      DOM_Event : Web.UI_Events.Mouse_Events.Mouse_Event
        := Event.As_Mouse_Event;
      UI_Event  : Web.UI.Events.Mouse.Mouse_Event;

   begin
      Web.UI.Events.Mouse.Constructors.Initialize (UI_Event, DOM_Event);
      Self.Owner.Mouse_Press_Event (UI_Event);
   end Handle_Event;

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event
    (Self  : in out Mouse_Up_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class)
   is
      DOM_Event : Web.UI_Events.Mouse_Events.Mouse_Event
        := Event.As_Mouse_Event;
      UI_Event  : Web.UI.Events.Mouse.Mouse_Event;

   begin
      Web.UI.Events.Mouse.Constructors.Initialize (UI_Event, DOM_Event);
      Self.Owner.Mouse_Release_Event (UI_Event);
   end Handle_Event;

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event
    (Self  : in out Mouse_Enter_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class)
   is
      DOM_Event : Web.UI_Events.Mouse_Events.Mouse_Event
        := Event.As_Mouse_Event;
      UI_Event  : Web.UI.Events.Mouse.Mouse_Event;

   begin
      Web.UI.Events.Mouse.Constructors.Initialize (UI_Event, DOM_Event);
      Self.Owner.Mouse_Enter_Event (UI_Event);
   end Handle_Event;

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event
    (Self  : in out Mouse_Leave_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class)
   is
      DOM_Event : Web.UI_Events.Mouse_Events.Mouse_Event
        := Event.As_Mouse_Event;
      UI_Event  : Web.UI.Events.Mouse.Mouse_Event;

   begin
      Web.UI.Events.Mouse.Constructors.Initialize (UI_Event, DOM_Event);
      Self.Owner.Mouse_Leave_Event (UI_Event);
   end Handle_Event;

--   ------------------
--   -- Handle_Event --
--   ------------------
--
--   overriding procedure Handle_Event
--    (Self  : not null access Wheel_Dispatcher;
--     Event : access WebAPI.DOM.Events.Event'Class)
--   is
--      E : WUI.Events.Mouse.Wheel.Mouse_Wheel_Event;
--
--   begin
--      WUI.Events.Mouse.Wheel.Constructors.Initialize
--       (E,
--        WebAPI.UI_Events.Wheel.Wheel_Event'Class (Event.all)'Unchecked_Access);
--      Self.Owner.Mouse_Wheel_Event (E);
--   end Handle_Event;

   -----------------
   -- Set_Visible --
   -----------------

   not overriding procedure Set_Visible
    (Self : in out Abstract_Widget;
     To   : Boolean) is
   begin
      Self.Element.Set_Hidden (not To);
   end Set_Visible;

end Web.UI.Widgets;
