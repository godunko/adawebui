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
private with Web.DOM.Events;
private with Web.DOM.Event_Listeners;
with Web.HTML.Elements;

with Web.Core.Connectables;
private with Web.Core.Connectables.Slots_0.Slots_1.Generic_Slots;
with Web.UI.Boolean_Slots;
with Web.UI.Events.Mouse;
--with WUI.Events.Mouse.Wheel;

package Web.UI.Widgets is

   type Abstract_Widget is
     abstract limited new Core.Connectables.Connectable_Object with private;

   type Widget_Access is access all Abstract_Widget'Class
     with Storage_Size => 0;

   not overriding procedure Set_Disabled
    (Self     : in out Abstract_Widget;
     Disabled : Boolean := True) is abstract;
   --  Available as slot.

   not overriding procedure Set_Enabled
    (Self    : in out Abstract_Widget;
     Enabled : Boolean := True) is abstract;
   --  Available as slot.

   not overriding procedure Set_Visible
    (Self : in out Abstract_Widget;
     To   : Boolean);
   --  Available as slot.

   -----------
   -- Slots --
   -----------

   function Set_Disabled_Slot
    (Self : in out Abstract_Widget'Class)
       return Web.UI.Boolean_Slots.Slot'Class;

   function Set_Enabled_Slot
    (Self : in out Abstract_Widget'Class)
       return Web.UI.Boolean_Slots.Slot'Class;

   function Set_Visible_Slot
    (Self : in out Abstract_Widget'Class)
       return Web.UI.Boolean_Slots.Slot'Class;

   not overriding procedure Mouse_Click_Event
    (Self  : in out Abstract_Widget;
     Event : in out Web.UI.Events.Mouse.Mouse_Event'Class) is null;

   not overriding procedure Mouse_Enter_Event
    (Self  : in out Abstract_Widget;
     Event : in out Web.UI.Events.Mouse.Mouse_Event'Class) is null;

   not overriding procedure Mouse_Leave_Event
    (Self  : in out Abstract_Widget;
     Event : in out Web.UI.Events.Mouse.Mouse_Event'Class) is null;

   not overriding procedure Mouse_Move_Event
    (Self  : in out Abstract_Widget;
     Event : in out Web.UI.Events.Mouse.Mouse_Event'Class) is null;

   not overriding procedure Mouse_Press_Event
    (Self  : in out Abstract_Widget;
     Event : in out Web.UI.Events.Mouse.Mouse_Event'Class) is null;

   not overriding procedure Mouse_Release_Event
    (Self  : in out Abstract_Widget;
     Event : in out Web.UI.Events.Mouse.Mouse_Event'Class) is null;

--   not overriding procedure Mouse_Wheel_Event
--    (Self  : in out Abstract_Widget;
--     Event : in out WUI.Events.Mouse.Wheel.Mouse_Wheel_Event'Class) is null;

   package Constructors is

      procedure Initialize
       (Self    : aliased in out Abstract_Widget'Class;
        Element : Web.HTML.Elements.HTML_Element'Class);

   end Constructors;

private

   -----------------------
   -- Event Dispatchers --
   -----------------------

   type Blur_Dispatcher
    (Owner : not null access Abstract_Widget'Class) is
       limited new Web.DOM.Event_Listeners.Event_Listener with null record;

   overriding procedure Handle_Event
    (Self  : in out Blur_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class);

   type Change_Dispatcher
    (Owner : not null access Abstract_Widget'Class) is
       limited new Web.DOM.Event_Listeners.Event_Listener with null record;

   overriding procedure Handle_Event
    (Self  : in out Change_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class);

   type Focus_Dispatcher
    (Owner : not null access Abstract_Widget'Class) is
       limited new Web.DOM.Event_Listeners.Event_Listener with null record;

   overriding procedure Handle_Event
    (Self  : in out Focus_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class);

   type Input_Dispatcher
    (Owner : not null access Abstract_Widget'Class) is
       limited new Web.DOM.Event_Listeners.Event_Listener with null record;

   overriding procedure Handle_Event
    (Self  : in out Input_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class);

   type Mouse_Click_Dispatcher
    (Owner : not null access Abstract_Widget'Class) is
       limited new Web.DOM.Event_Listeners.Event_Listener with null record;

   overriding procedure Handle_Event
    (Self  : in out Mouse_Click_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class);

   type Mouse_Move_Dispatcher
    (Owner : not null access Abstract_Widget'Class) is
       limited new Web.DOM.Event_Listeners.Event_Listener with null record;

   overriding procedure Handle_Event
    (Self  : in out Mouse_Move_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class);

   type Mouse_Down_Dispatcher
    (Owner : not null access Abstract_Widget'Class) is
       limited new Web.DOM.Event_Listeners.Event_Listener with null record;

   overriding procedure Handle_Event
    (Self  : in out Mouse_Down_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class);

   type Mouse_Up_Dispatcher
    (Owner : not null access Abstract_Widget'Class) is
       limited new Web.DOM.Event_Listeners.Event_Listener with null record;

   overriding procedure Handle_Event
    (Self  : in out Mouse_Up_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class);

   type Mouse_Enter_Dispatcher
    (Owner : not null access Abstract_Widget'Class) is
       limited new Web.DOM.Event_Listeners.Event_Listener with null record;

   overriding procedure Handle_Event
    (Self  : in out Mouse_Enter_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class);

   type Mouse_Leave_Dispatcher
    (Owner : not null access Abstract_Widget'Class) is
       limited new Web.DOM.Event_Listeners.Event_Listener with null record;

   overriding procedure Handle_Event
    (Self  : in out Mouse_Leave_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class);

--   type Wheel_Dispatcher
--    (Owner : not null access Abstract_Widget'Class) is
--       limited new WebAPI.DOM.Event_Listeners.Event_Listener with null record;
--
--   overriding procedure Handle_Event
--    (Self  : not null access Wheel_Dispatcher;
--     Event : access WebAPI.DOM.Events.Event'Class);

   ---------------------
   -- Abstract_Widget --
   ---------------------

   type Abstract_Widget is
     abstract limited new Core.Connectables.Connectable_Object with
   record
      Element     : Web.HTML.Elements.HTML_Element;
      Click       : aliased
        Mouse_CLick_Dispatcher (Abstract_Widget'Unchecked_Access);
      Mouse_Move  : aliased
        Mouse_Move_Dispatcher (Abstract_Widget'Unchecked_Access);
      Mouse_Down  : aliased
        Mouse_Down_Dispatcher (Abstract_Widget'Unchecked_Access);
      Mouse_Up    : aliased
        Mouse_Up_Dispatcher (Abstract_Widget'Unchecked_Access);
      Mouse_Enter : aliased
        Mouse_Enter_Dispatcher (Abstract_Widget'Unchecked_Access);
      Mouse_Leave : aliased
        Mouse_Leave_Dispatcher (Abstract_Widget'Unchecked_Access);
--      Wheel      : aliased Wheel_Dispatcher (Abstract_Widget'Unchecked_Access);
      Change      : aliased
        Change_Dispatcher (Abstract_Widget'Unchecked_Access);
      Input       : aliased
        Input_Dispatcher (Abstract_Widget'Unchecked_Access);
      Blur        : aliased Blur_Dispatcher (Abstract_Widget'Unchecked_Access);
      Focus       : aliased Focus_Dispatcher (Abstract_Widget'Unchecked_Access);
   end record;

   not overriding procedure Focus_In_Event (Self : in out Abstract_Widget);

   not overriding procedure Focus_Out_Event (Self : in out Abstract_Widget);

   not overriding procedure Input_Event
    (Self : in out Abstract_Widget) is null;

   not overriding procedure Change_Event
    (Self : in out Abstract_Widget) is null;

   -----------
   -- Slots --
   -----------

   package Set_Disabled_Slots is
     new Web.UI.Boolean_Slots.Generic_Slots (Abstract_Widget, Set_Disabled);

   function Set_Disabled_Slot
    (Self : in out Abstract_Widget'Class)
       return Web.UI.Boolean_Slots.Slot'Class
         renames Set_Disabled_Slots.To_Slot;

   package Set_Enabled_Slots is
     new Web.UI.Boolean_Slots.Generic_Slots (Abstract_Widget, Set_Enabled);

   function Set_Enabled_Slot
    (Self : in out Abstract_Widget'Class)
       return Web.UI.Boolean_Slots.Slot'Class
         renames Set_Enabled_Slots.To_Slot;

   package Set_Visible_Slots is
     new Web.UI.Boolean_Slots.Generic_Slots (Abstract_Widget, Set_Visible);

   function Set_Visible_Slot
    (Self : in out Abstract_Widget'Class)
       return Web.UI.Boolean_Slots.Slot'Class
         renames Set_Visible_Slots.To_Slot;

end Web.UI.Widgets;
