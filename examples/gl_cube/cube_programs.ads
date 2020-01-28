
with OpenGL.Generic_Buffers;
with OpenGL.Programs;

package Cube_Programs is

   type Vertex_Data is record
      Vertex_Position : OpenGL.GLfloat_Vector_3;
      Vertex_Color    : OpenGL.GLfloat_Vector_3;
   end record;

   type Vertex_Data_Array is array (Positive range <>) of Vertex_Data;

   package Vertex_Data_Buffers is
     new OpenGL.Generic_Buffers (Vertex_Data, Positive, Vertex_Data_Array);

   type Index_Data_Array is array (Positive range <>) of OpenGL.GLushort;

   package Index_Data_Buffers is
     new OpenGL.Generic_Buffers (OpenGL.GLushort, Positive, Index_Data_Array);

   type Cube_Program is new OpenGL.Programs.OpenGL_Program with private;

   procedure Initialize (Self : in out Cube_Program'Class);
   --  Initialize program object.

   procedure Set_Vertex_Data_Buffer
    (Self   : in out Cube_Program'Class;
     Buffer : Vertex_Data_Buffers.OpenGL_Buffer'Class);
   --  Sets buffer with data to draw.

   procedure Set_Projection_Matrix
    (Self   : in out Cube_Program'Class;
     Matrix : OpenGL.GLfloat_Matrix_4x4);
   --  Sets projection matrix to be used to draw.

   procedure Set_Model_View_Matrix
    (Self   : in out Cube_Program'Class;
     Matrix : OpenGL.GLfloat_Matrix_4x4);
   --  Sets model view matrix to be used to draw.

   procedure Set_Model_Move_Matrix
    (Self   : in out Cube_Program'Class;
     Matrix : OpenGL.GLfloat_Matrix_4x4);
   --  Sets model move matrix to be used to draw.

private

   type Cube_Program is new OpenGL.Programs.OpenGL_Program with record
      PM  : OpenGL.Uniform_Location;
      MVM : OpenGL.Uniform_Location;
      MMM : OpenGL.Uniform_Location;
      VP  : OpenGL.Attribute_Location;
      CP  : OpenGL.Attribute_Location;
   end record;

   overriding function Link (Self : in out Cube_Program) return Boolean;
   --  Executed at link time. Link shaders into program and extracts locations
   --  of attributes.

end Cube_Programs;
