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
--  $Revision: 5687 $ $Date: 2017-01-12 15:33:13 +0300 (Thu, 12 Jan 2017) $
------------------------------------------------------------------------------

with Web.Window;

package body Web.UI.Widgets.GL_Widgets is

   type GL_Widget_Access is access all Abstract_GL_Widget'Class;

   Instance         : GL_Widget_Access;
   --  XXX Only one instance of Abstract_GL_Widget may be created for now.
   Frame_Request_Id : Web.DOM_Unsigned_Long := 0;
   --  Identifier of registered request of animation frame, then requested.
   --  If there is no active requests it is equal to zero.

   procedure Animation_Frame_Request_Handler
     (Time : Web.DOM_High_Res_Time_Stamp)
       with Convention => C;
   --  Calls Animation_Frame procedure for all instances of Abstract_GL_Widget.

   procedure Animation_Frame (Self : in out Abstract_GL_Widget'Class);
   --  Handles animation frame and requests next animation frame. Animation
   --  frames is used to do most of GL related processing, including:
   --   - finish of initialization of widget's GL code;
   --   - detection of resize of canvas;
   --   - redrawing after finish of initialization, on detected resize of the
   --     canvas and on request.

   ---------------------
   -- Animation_Frame --
   ---------------------

   procedure Animation_Frame (Self : in out Abstract_GL_Widget'Class) is
      Ratio          : constant Web.DOM_Double
        := Web.Window.Get_Device_Pixel_Ratio;
      Display_Width  : constant Web.DOM_Unsigned_Long
        := Web.DOM_Unsigned_Long
            (Web.DOM_Double'Floor
              (Web.DOM_Double (Self.Canvas.Get_Client_Width) * Ratio));
      Display_Height : constant Web.DOM_Unsigned_Long
        := Web.DOM_Unsigned_Long
            (Web.DOM_Double'Floor
              (Web.DOM_Double (Self.Canvas.Get_Client_Height) * Ratio));
      Resize_Needed  : constant Boolean
        := not Self.Initialized
             or Self.Canvas.Get_Width /= Display_Width
             or Self.Canvas.Get_Height /= Display_Height;
      --  Whether Resize_GL should be called due to change of canvas size.

   begin
      Self.Context.Make_Current;

      --  Finish initialization of the widget's GL code.

      if not Self.Initialized then
         Self.Initialized := True;
         Abstract_GL_Widget'Class (Self).Initialize_GL;
      end if;

      --  Complete resize of canvas and notify widget.

      if Resize_Needed then
         Self.Canvas.Set_Width (Display_Width);
         Self.Canvas.Set_Height (Display_Height);

         Self.Functions.Viewport
          (X      => 0,
           Y      => 0,
           Width  => OpenGL.GLsizei (Self.Canvas.Get_Width),
           Height => OpenGL.GLsizei (Self.Canvas.Get_Height));

         Abstract_GL_Widget'Class (Self).Resize_GL
          (Integer (Self.Canvas.Get_Width), Integer (Self.Canvas.Get_Height));
      end if;

      --  Redraw content of canvas when necessary.

      if Resize_Needed or Self.Redraw_Needed then
         Self.Redraw_Needed := False;
         Abstract_GL_Widget'Class (Self).Paint_GL;
         Self.Functions.Flush;
      end if;
   end Animation_Frame;

   -------------------------------------
   -- Animation_Frame_Request_Handler --
   -------------------------------------

   procedure Animation_Frame_Request_Handler
     (Time : Web.DOM_High_Res_Time_Stamp) is
   begin
      Frame_Request_Id := 0;
      --  Frame request has been completed.

      Instance.Animation_Frame;
   end Animation_Frame_Request_Handler;

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
       (Self   : in out Abstract_GL_Widget'Class;
        Canvas : in out Web.HTML.Canvases.HTML_Canvas_Element'Class) is
      begin
         Web.UI.Widgets.Constructors.Initialize (Self, Canvas);

         Self.Canvas := Web.HTML.Canvases.HTML_Canvas_Element (Canvas);

         Self.Canvas.Add_Event_Listener
          (+"webglcontextlost", Self.Lost'Unchecked_Access, False);
         Self.Canvas.Add_Event_Listener
          (+"webglcontextrestored", Self.Lost'Unchecked_Access, False);

         Self.Context := new OpenGL.Contexts.OpenGL_Context;
         Self.Context.Create (Canvas);
         Self.Context.Make_Current;

         Instance := Self'Unchecked_Access;

         Self.Update;
      end Initialize;

   end Constructors;

   ---------------
   -- Functions --
   ---------------

   function Functions
    (Self : in out Abstract_GL_Widget'Class)
       return access OpenGL.Functions.OpenGL_Functions'Class is
   begin
      return Self.Context.Functions;
   end Functions;

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event
    (Self  : in out Context_Lost_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class) is
   begin
      Self.Owner.Context_Lost;
   end Handle_Event;

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event
    (Self  : in out Context_Restored_Dispatcher;
     Event : in out Web.DOM.Events.Event'Class) is
   begin
      Self.Owner.Context_Restored;
   end Handle_Event;

   ------------
   -- Update --
   ------------

   procedure Update (Self : in out Abstract_GL_Widget) is
   begin
      --  Request redraw on processing of next animation frame.

      Self.Redraw_Needed := True;

      if Frame_Request_Id = 0 then
         --  Register request of animation frame, if not registered.
         --  Initialization of the GL related features will be continued
         --  durin handling of the requested animation frame.

         Frame_Request_Id :=
           Web.Window.Request_Animation_Frame
            (Animation_Frame_Request_Handler'Access);
      end if;
   end Update;

end Web.UI.Widgets.GL_Widgets;
