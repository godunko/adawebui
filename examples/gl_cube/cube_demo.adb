
with Ada.Numerics.Generic_Elementary_Functions;
--  with System.IO;

with Web.HTML.Canvases;
with Web.Strings;
with Web.UI.Events.Mouse.Click;
with Web.UI.GL_Widgets;
with Web.Window;

with OpenGL;

with Cube_Programs;

package body Cube_Demo is

   use type OpenGL.GLfloat;

   function "+" (Item : Wide_Wide_String) return Web.Strings.Web_String
     renames Web.Strings.To_Web_String;

   package GLfloat_Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (OpenGL.GLfloat);

   procedure Initialize_Demo;

   type My_GL_Widget is
     new Web.UI.GL_Widgets.Abstract_GL_Widget with record
      Vertex_Buffer : Cube_Programs.Vertex_Data_Buffers.OpenGL_Buffer (OpenGL.Vertex);
      Index_Buffer  : Cube_Programs.Index_Data_Buffers.OpenGL_Buffer (OpenGL.Index);
      Program       : Cube_Programs.Cube_Program;
   end record;

   type My_GL_Widget_Access is access all My_GL_Widget'Class;

   overriding procedure Initialize_GL (Self : in out My_GL_Widget);

   overriding procedure Paint_GL (Self : in out My_GL_Widget);

   overriding procedure Resize_GL
    (Self   : in out My_GL_Widget;
     Width  : Integer;
     Height : Integer);

   overriding procedure Context_Lost (Self : in out My_GL_Widget);

--   overriding procedure Click_Event
--    (Self  : in out My_GL_Widget;
--     Event : in out Web.UI.Events.Mouse.Click.Click_Event'Class);

   procedure Initialize (Self : in out My_GL_Widget'Class);

   Points : constant Cube_Programs.Vertex_Data_Array
     := ((Vertex_Position => (-1.0, -1.0, -1.0), Vertex_Color => (5.0, 3.0, 7.0)),
         (Vertex_Position => ( 1.0, -1.0, -1.0), Vertex_Color => (5.0, 3.0, 7.0)),
         (Vertex_Position => ( 1.0,  1.0, -1.0), Vertex_Color => (5.0, 3.0, 7.0)),
         (Vertex_Position => (-1.0,  1.0, -1.0), Vertex_Color => (5.0, 3.0, 7.0)),
         (Vertex_Position => (-1.0, -1.0,  1.0), Vertex_Color => (1.0, 1.0, 3.0)),
         (Vertex_Position => ( 1.0, -1.0,  1.0), Vertex_Color => (1.0, 1.0, 3.0)),
         (Vertex_Position => ( 1.0,  1.0,  1.0), Vertex_Color => (1.0, 1.0, 3.0)),
         (Vertex_Position => (-1.0,  1.0,  1.0), Vertex_Color => (1.0, 1.0, 3.0)),
         (Vertex_Position => (-1.0, -1.0, -1.0), Vertex_Color => (0.0, 0.0, 1.0)),
         (Vertex_Position => (-1.0,  1.0, -1.0), Vertex_Color => (0.0, 0.0, 1.0)),
         (Vertex_Position => (-1.0,  1.0,  1.0), Vertex_Color => (0.0, 0.0, 1.0)),
         (Vertex_Position => (-1.0, -1.0,  1.0), Vertex_Color => (0.0, 0.0, 1.0)),
         (Vertex_Position => ( 1.0, -1.0, -1.0), Vertex_Color => (1.0, 0.0, 0.0)),
         (Vertex_Position => ( 1.0,  1.0, -1.0), Vertex_Color => (1.0, 0.0, 0.0)),
         (Vertex_Position => ( 1.0,  1.0,  1.0), Vertex_Color => (1.0, 0.0, 0.0)),
         (Vertex_Position => ( 1.0, -1.0,  1.0), Vertex_Color => (1.0, 0.0, 0.0)),
         (Vertex_Position => (-1.0, -1.0, -1.0), Vertex_Color => (1.0, 1.0, 0.0)),
         (Vertex_Position => (-1.0, -1.0,  1.0), Vertex_Color => (1.0, 1.0, 0.0)),
         (Vertex_Position => ( 1.0, -1.0,  1.0), Vertex_Color => (1.0, 1.0, 0.0)),
         (Vertex_Position => ( 1.0, -1.0, -1.0), Vertex_Color => (1.0, 1.0, 0.0)),
         (Vertex_Position => (-1.0,  1.0, -1.0), Vertex_Color => (0.0, 1.0, 0.0)),
         (Vertex_Position => (-1.0,  1.0,  1.0), Vertex_Color => (0.0, 1.0, 0.0)),
         (Vertex_Position => ( 1.0,  1.0,  1.0), Vertex_Color => (0.0, 1.0, 0.0)),
         (Vertex_Position => ( 1.0,  1.0, -1.0), Vertex_Color => (0.0, 1.0, 0.0)));

   Indicies : constant Cube_Programs.Index_Data_Array
     := ( 0,  1,  2,   0,  2,  3,   4,  5,  6,   4,  6,  7,
          8,  9, 10,   8, 10, 11,  12, 13, 14,  12, 14, 15,
         16, 17, 18,  16, 18, 19,  20, 21, 22,  20, 22, 23);

   function Projection
    (Angle  : OpenGL.GLfloat;
     Aspect : OpenGL.GLfloat;
     Z_Min  : OpenGL.GLfloat;
     Z_Max  : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4;

   ----------------
   -- Projection --
   ----------------

   function Projection
    (Angle  : OpenGL.GLfloat;
     Aspect : OpenGL.GLfloat;
     Z_Min  : OpenGL.GLfloat;
     Z_Max  : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4
   is
      Ang : constant OpenGL.GLfloat
        := GLfloat_Elementary_Functions.Tan
            ((Angle * 0.5) * Ada.Numerics.PI / 180.0);

   begin
     return
      ((0.5 / Ang, 0.0, 0.0, 0.0),
       (0.0, 0.5 * Aspect / Ang, 0.0, 0.0),
       (0.0, 0.0, -(Z_Max + Z_Min) / (Z_Max - Z_Min), (-2.0 * Z_Max * Z_Min) / (Z_Max - Z_Min)),
       (0.0, 0.0, -1.0, 0.0));
   end Projection;

   procedure Rotate_X
    (M     : in out OpenGL.GLfloat_Matrix_4x4;
     Angle : OpenGL.GLfloat);

   procedure Rotate_Y
    (M     : in out OpenGL.GLfloat_Matrix_4x4;
     Angle : OpenGL.GLfloat);

   procedure Rotate_Z
    (M     : in out OpenGL.GLfloat_Matrix_4x4;
     Angle : OpenGL.GLfloat);

   --------------
   -- Rotate_X --
   --------------

   procedure Rotate_X
    (M     : in out OpenGL.GLfloat_Matrix_4x4;
     Angle : OpenGL.GLfloat)
   is
      --    0  4  8 12
      --    1  5  9 13
      --    2  6 10 14
      --    3  7 11 15

      C   : constant OpenGL.GLfloat := GLfloat_Elementary_Functions.Cos (Angle);
      S   : constant OpenGL.GLfloat := GLfloat_Elementary_Functions.Sin (Angle);
      M21 : constant OpenGL.GLfloat := M (2, 1);
      M22 : constant OpenGL.GLfloat := M (2, 2);
      M23 : constant OpenGL.GLfloat := M (2, 3);
--            var mv1 = m[1], mv5 = m[5], mv9 = m[9];

   begin
      M (2, 1) := C * M (2, 1) - S * M (3, 1);
--            m[1] = m[1]*c-m[2]*s;
      M (2, 2) := C * M (2, 2) - S * M (3, 2);
--            m[5] = m[5]*c-m[6]*s;
      M (2, 3) := C * M (2, 3) - S * M (3, 3);
--            m[9] = m[9]*c-m[10]*s;
      M (3, 1) := C * M (3, 1) + S * M21;
--            m[2] = m[2]*c+mv1*s;
      M (3, 2) := C * M (3, 2) + S * M22;
--            m[6] = m[6]*c+mv5*s;
      M (3, 3) := C * M (3, 3) + S * M23;
--            m[10] = m[10]*c+mv9*s;
   end Rotate_X;

   --------------
   -- Rotate_Y --
   --------------

   procedure Rotate_Y
    (M     : in out OpenGL.GLfloat_Matrix_4x4;
     Angle : OpenGL.GLfloat)
   is
      --    0  4  8 12
      --    1  5  9 13
      --    2  6 10 14
      --    3  7 11 15

      C   : constant OpenGL.GLfloat := GLfloat_Elementary_Functions.Cos (Angle);
      S   : constant OpenGL.GLfloat := GLfloat_Elementary_Functions.Sin (Angle);
      M11 : constant OpenGL.GLfloat := M (1, 1);
      M12 : constant OpenGL.GLfloat := M (1, 2);
      M13 : constant OpenGL.GLfloat := M (1, 3);
--            var mv0 = m[0], mv4 = m[4], mv8 = m[8];

   begin
      M (1, 1) := C * M (1, 1) + S * M (3, 1);
--            m[0] = c*m[0]+s*m[2];
      M (1, 2) := C * M (1, 2) + S * M (3, 2);
--            m[4] = c*m[4]+s*m[6];
      M (1, 3) := C * M (1, 3) + S * M (3, 3);
--            m[8] = c*m[8]+s*m[10];
      M (3, 1) := C * M (3, 1) - S * M11;
--            m[2] = c*m[2]-s*mv0;
      M (3, 2) := C * M (3, 2) - S * M12;
--            m[6] = c*m[6]-s*mv4;
      M (3, 3) := C * M (3, 3) - S * M13;
--            m[10] = c*m[10]-s*mv8;
   end Rotate_Y;

   --------------
   -- Rotate_Z --
   --------------

   procedure Rotate_Z
    (M     : in out OpenGL.GLfloat_Matrix_4x4;
     Angle : OpenGL.GLfloat)
   is
      --    0  4  8 12
      --    1  5  9 13
      --    2  6 10 14
      --    3  7 11 15

      C   : constant OpenGL.GLfloat := GLfloat_Elementary_Functions.Cos (Angle);
      S   : constant OpenGL.GLfloat := GLfloat_Elementary_Functions.Sin (Angle);
      M11 : constant OpenGL.GLfloat := M (1, 1);
      M12 : constant OpenGL.GLfloat := M (1, 2);
      M13 : constant OpenGL.GLfloat := M (1, 3);
--            var mv0 = m[0], mv4 = m[4], mv8 = m[8];

   begin
      M (1, 1) := C * M (1, 1) - S * M (2, 1);
--            m[0] = c*m[0]-s*m[1];
      M (1, 2) := C * M (1, 2) - S * M (2, 2);
--            m[4] = c*m[4]-s*m[5];
      M (1, 3) := C * M (1, 3) - S * M (2, 3);
--            m[8] = c*m[8]-s*m[9];
      M (2, 1) := C * M (2, 1) + S * M11;
--            m[1]=c*m[1]+s*mv0;
      M (2, 2) := C * M (2, 2) + S * M12;
--            m[5]=c*m[5]+s*mv4;
      M (2, 3) := C * M (2, 3) + S * M13;
--            m[9]=c*m[9]+s*mv8;
   end Rotate_Z;

   Projection_Matrix : OpenGL.GLfloat_Matrix_4x4;

   Model_View_Matrix : constant OpenGL.GLfloat_Matrix_4x4
     := ((1.0, 0.0, 0.0, 0.0),
         (0.0, 1.0, 0.0, 0.0),
         (0.0, 0.0, 1.0, -6.0),
         (0.0, 0.0, 0.0, 1.0));

   Model_Move_Matrix : OpenGL.GLfloat_Matrix_4x4
     := ((1.0, 0.0, 0.0, 0.0),
         (0.0, 1.0, 0.0, 0.0),
         (0.0, 0.0, 1.0, 0.0),
         (0.0, 0.0, 0.0, 1.0));

   W : My_GL_Widget_Access;
--   T : OpenGL.GLfloat := 0.0;

--   -----------------
--   -- Click_Event --
--   -----------------
--
--   overriding procedure Click_Event
--    (Self  : in out My_GL_Widget;
--     Event : in out Web.UI.Events.Mouse.Click.Click_Event'Class)
--   is
--   begin
--      System.IO.Put_Line ("Clicked!");
--   end Click_Event;

   ------------------
   -- Context_Lost --
   ------------------

   overriding procedure Context_Lost (Self : in out My_GL_Widget) is
   begin
      raise Program_Error;
   end Context_Lost;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out My_GL_Widget'Class) is
      Canvas : Web.HTML.Canvases.HTML_Canvas_Element
        := Web.Window.Document.Get_Element_By_Id
            (+"cube_gl_canvas").As_HTML_Canvas;

   begin
      Web.UI.GL_Widgets.Constructors.Initialize (Self, Canvas);
   end Initialize;

   ---------------------
   -- Initialize_Demo --
   ---------------------

   procedure Initialize_Demo is
   begin
      W := new My_GL_Widget;
      Initialize (W.all);
   end Initialize_Demo;

   -------------------
   -- Initialize_GL --
   -------------------

   overriding procedure Initialize_GL (Self : in out My_GL_Widget) is
   begin
      Self.Vertex_Buffer.Create;
      Self.Vertex_Buffer.Bind;
      Self.Vertex_Buffer.Allocate (Points);

      Self.Index_Buffer.Create;
      Self.Index_Buffer.Bind;
      Self.Index_Buffer.Allocate (Indicies);

      Self.Program.Initialize;
   end Initialize_GL;

   --------------
   -- Paint_GL --
   --------------

   overriding procedure Paint_GL (Self : in out My_GL_Widget) is
      use type OpenGL.GLbitfield;

      DT : OpenGL.GLfloat := 1.0 / 60.0 * 1_000.0;

   begin
      Rotate_Z (Model_Move_Matrix, DT * 0.005);
      Rotate_Y (Model_Move_Matrix, DT * 0.002);
      Rotate_X (Model_Move_Matrix, DT * 0.003);

      Self.Program.Bind;
      Self.Program.Set_Projection_Matrix (Projection_Matrix);
      Self.Program.Set_Model_View_Matrix (Model_View_Matrix);
      Self.Program.Set_Model_Move_Matrix (Model_Move_Matrix);
      Self.Program.Set_Vertex_Data_Buffer (Self.Vertex_Buffer);

      Self.Functions.Enable (OpenGL.GL_DEPTH_TEST);
      Self.Functions.Depth_Func (OpenGL.GL_LEQUAL);
      Self.Functions.Clear_Color (0.5, 0.5, 0.5, 0.9);
      Self.Functions.Clear_Depth (1.0);
      Self.Functions.Clear
       (OpenGL.GL_COLOR_BUFFER_BIT + OpenGL.GL_DEPTH_BUFFER_BIT);

      Self.Vertex_Buffer.Bind;
      Self.Index_Buffer.Bind;
      Self.Functions.Draw_Elements
       (OpenGL.GL_TRIANGLES, Indicies'Length, OpenGL.GL_UNSIGNED_SHORT, 0);

      Self.Update;
   end Paint_GL;

   ---------------
   -- Resize_GL --
   ---------------

   overriding procedure Resize_GL
    (Self   : in out My_GL_Widget;
     Width  : Integer;
     Height : Integer) is
   begin
      Projection_Matrix :=
        Projection
         (40.0, OpenGL.GLfloat (Width) / OpenGL.GLfloat (Height), 1.0, 100.0);
   end Resize_GL;

begin
   Initialize_Demo;
end Cube_Demo;
