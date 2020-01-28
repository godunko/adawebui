
with Web.HTML.Scripts;
with Web.Window;
with Web.Strings;

package body Cube_Programs is

   function "+" (Item : Wide_Wide_String) return Web.Strings.Web_String
     renames Web.Strings.To_Web_String;

--   GS : constant League.Strings.Universal_String :=
--     League.Strings.To_Universal_String ("gSampler");
--   VP : constant League.Strings.Universal_String :=
--     League.Strings.To_Universal_String ("vp");
--   TC : constant League.Strings.Universal_String :=
--     League.Strings.To_Universal_String ("tc");

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Cube_Program'Class) is
   begin
      Self.Add_Shader_From_Source_Code
       (OpenGL.Vertex,
        Web.Window.Document.Get_Element_By_Id
         (+"cube-vertex-shader").As_HTML_Script.Get_Text);
      Self.Add_Shader_From_Source_Code
       (OpenGL.Fragment,
        Web.Window.Document.Get_Element_By_Id
         (+"cube-fragment-shader").As_HTML_Script.Get_Text);
   end Initialize;

   ----------
   -- Link --
   ----------

   overriding function Link (Self : in out Cube_Program) return Boolean is
   begin
      if not OpenGL.Programs.OpenGL_Program (Self).Link then
         return False;
      end if;

      Self.PM  := Self.Uniform_Location (+"uProjectionMatrix");
      Self.MVM := Self.Uniform_Location (+"uModelViewMatrix");
      Self.MMM := Self.Uniform_Location (+"uModelMoveMatrix");
      Self.VP  := Self.Attribute_Location (+"aVertexPosition");
      Self.CP  := Self.Attribute_Location (+"aVertexColor");

      return True;
   end Link;

   ---------------------------
   -- Set_Model_Move_Matrix --
   ---------------------------

   procedure Set_Model_Move_Matrix
    (Self   : in out Cube_Program'Class;
     Matrix : OpenGL.GLfloat_Matrix_4x4) is
   begin
      Self.Set_Uniform_Value (Self.MMM, Matrix);
   end Set_Model_Move_Matrix;

   ---------------------------
   -- Set_Model_View_Matrix --
   ---------------------------

   procedure Set_Model_View_Matrix
    (Self   : in out Cube_Program'Class;
     Matrix : OpenGL.GLfloat_Matrix_4x4) is
   begin
      Self.Set_Uniform_Value (Self.MVM, Matrix);
   end Set_Model_View_Matrix;

   ---------------------------
   -- Set_Projection_Matrix --
   ---------------------------

   procedure Set_Projection_Matrix
    (Self   : in out Cube_Program'Class;
     Matrix : OpenGL.GLfloat_Matrix_4x4) is
   begin
      Self.Set_Uniform_Value (Self.PM, Matrix);
   end Set_Projection_Matrix;

   ----------------------------
   -- Set_Vertex_Data_Buffer --
   ----------------------------

   procedure Set_Vertex_Data_Buffer
    (Self   : in out Cube_Program'Class;
     Buffer : Vertex_Data_Buffers.OpenGL_Buffer'Class)
   is
      Dummy : Vertex_Data;

   begin
      Self.Enable_Attribute_Array (Self.VP);
      Self.Enable_Attribute_Array (Self.CP);

      Self.Set_Attribute_Buffer
       (Location   => Self.VP,
        Data_Type  => OpenGL.GL_FLOAT,
        Tuple_Size => Dummy.Vertex_Position'Length,
        Offset     => Dummy.Vertex_Position'Position,
        Stride     => Vertex_Data_Buffers.Stride);
      Self.Set_Attribute_Buffer
       (Location   => Self.CP,
        Data_Type  => OpenGL.GL_FLOAT,
        Tuple_Size => Dummy.Vertex_Color'Length,
        Offset     => Dummy.Vertex_Color'Position,
        Stride     => Vertex_Data_Buffers.Stride);
   end Set_Vertex_Data_Buffer;

end Cube_Programs;
