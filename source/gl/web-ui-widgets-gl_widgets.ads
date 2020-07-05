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
--  $Revision: 5682 $ $Date: 2017-01-11 00:55:53 +0300 (Wed, 11 Jan 2017) $
------------------------------------------------------------------------------
--  Abstract_GL_Widget provides integration between implementation of OpenGL
--  API and application.
------------------------------------------------------------------------------

with Web.HTML.Canvases;

private with OpenGL.Contexts;
with OpenGL.Functions;

package Web.UI.Widgets.GL_Widgets is

   type Abstract_GL_Widget is
     abstract new Web.UI.Widgets.Abstract_Widget with private;

   function Functions
    (Self : in out Abstract_GL_Widget'Class)
       return access OpenGL.Functions.OpenGL_Functions'Class;
   --  Returns OpenGL functions to be used with OpenGL context of widget.

   procedure Update (Self : in out Abstract_GL_Widget);
   --  Requests redraw.

   not overriding procedure Initialize_GL
    (Self : in out Abstract_GL_Widget) is abstract;
   --  This dispatching subprogram is called once before the first call to
   --  Paint_GL. Reimplement it in a derived type.
   --
   --  This subprogram should set up any required OpenGL resources and state. 
   --
   --  The framebuffer is not yet available at this stage, so avoid issuing
   --  draw calls from here.

   not overriding procedure Paint_GL
    (Self : in out Abstract_GL_Widget) is abstract;
   --  This dispatching subprogram is called whenever the widget needs to be
   --  painted. Reimplement it in a derived type.
   --
   --  Current context is set to context of this widget.
   --
   --  Before invoking this function, the context and the framebuffer are
   --  bound, and the viewport is set up by a call to glViewport. No other
   --  state is set and no clearing or drawing is performed by the framework.

   not overriding procedure Resize_GL
    (Self   : in out Abstract_GL_Widget;
     Width  : Integer;
     Height : Integer) is null;
   --  This dispatching subprogram is called whenever the widget has been
   --  resized. Reimplement it in a subclass. The new size is passed in Width
   --  and Height.

   not overriding procedure Context_Lost
    (Self : in out Abstract_GL_Widget) is null;

   not overriding procedure Context_Restored
    (Self : in out Abstract_GL_Widget) is null;

   overriding procedure Set_Disabled
    (Self     : in out Abstract_GL_Widget;
     Disabled : Boolean := True) is null;

   package Constructors is

      procedure Initialize
       (Self   : in out Abstract_GL_Widget'Class;
        Canvas : in out Web.HTML.Canvases.HTML_Canvas_Element'Class);

   end Constructors;

private

   type Context_Lost_Dispatcher
    (Owner : not null access Abstract_GL_Widget'Class) is
       limited new Web.DOM.Event_Listeners.Event_Listener with null record;

   overriding procedure Handle_Event
    (Self  : in out Context_Lost_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class);

   type Context_Restored_Dispatcher
    (Owner : not null access Abstract_GL_Widget'Class) is
       limited new Web.DOM.Event_Listeners.Event_Listener with null record;

   overriding procedure Handle_Event
    (Self  : in out Context_Restored_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class);

   type Abstract_GL_Widget is
     abstract new Web.UI.Widgets.Abstract_Widget with record
      Canvas        : Web.HTML.Canvases.HTML_Canvas_Element;
      Context       : OpenGL.Contexts.OpenGL_Context_Access;
      Lost          : aliased
        Context_Lost_Dispatcher (Abstract_GL_Widget'Unchecked_Access);
      Restored      : aliased
        Context_Restored_Dispatcher (Abstract_GL_Widget'Unchecked_Access);
      Initialized   : Boolean := False;
      Redraw_Needed : Boolean := True;
      --  Redraw should be done on next handling of animation frame.
   end record;

end Web.UI.Widgets.GL_Widgets;
