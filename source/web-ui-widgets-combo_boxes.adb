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

package body Web.UI.Widgets.Combo_Boxes is

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

--      type Combo_Box_Internal_Access is access all Abstract_Combo_Box'Class;

--      ------------
--      -- Create --
--      ------------
--
--      function Create
--       (Element :
--          not null WebAPI.HTML.Select_Elements.HTML_Select_Element_Access)
--            return not null Combo_Box_Access
--      is
--         Result : constant not null Combo_Box_Internal_Access
--           := new Combo_Box;
--
--      begin
--         Initialize (Result.all, Element);
--
--         return Combo_Box_Access (Result);
--      end Create;
--
--      ------------
--      -- Create --
--      ------------
--
--      function Create
--       (Id : League.Strings.Universal_String)
--          return not null Combo_Box_Access is
--      begin
--         return
--           Create
--            (WebAPI.HTML.Select_Elements.HTML_Select_Element_Access
--              (WebAPI.HTML.Globals.Window.Get_Document.Get_Element_By_Id
--                (Id)));
--      end Create;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
       (Self    : in out Abstract_Combo_Box'Class;
        Element : Web.HTML.Selects.HTML_Select_Element'Class) is
      begin
         Web.UI.Widgets.Constructors.Initialize (Self, Element);
      end Initialize;

   end Constructors;

   ------------------
   -- Change_Event --
   ------------------

   overriding procedure Change_Event (Self : in out Abstract_Combo_Box) is
   begin
      Self.Current_Index_Changed.Emit
       (Integer (Self.Element.As_HTML_Select.Get_Selected_Index + 1));
   end Change_Event;

--   -------------------
--   -- Current_Index --
--   -------------------
--
--   function Current_Index
--    (Self : in out Combo_Box'Class)
--       return League.Strings.Universal_String
--   is
--      Input : constant WebAPI.HTML.Select_Elements.HTML_Select_Element_Access
--        := WebAPI.HTML.Select_Elements.HTML_Select_Element_Access
--            (Self.Element);
--
--   begin
--      return Input.Get_Value;
--   end Current_Index;

   ----------------------------------
   -- Current_Index_Changed_Signal --
   ----------------------------------

   not overriding function Current_Index_Changed_Signal
    (Self : in out Abstract_Combo_Box)
       return not null access Web.UI.Integer_Slots.Signal'Class is
   begin
      return Self.Current_Index_Changed'Unchecked_Access;
   end Current_Index_Changed_Signal;

--   -----------------
--   -- Input_Event --
--   -----------------
--
--   overriding procedure Input_Event (Self : in out Combo_Box) is
--      Input : constant WebAPI.HTML.Select_Elements.HTML_Select_Element_Access
--        := WebAPI.HTML.Select_Elements.HTML_Select_Element_Access
--            (Self.Element);
--
--   begin
--      Self.Current_Index_Changed.Emit (Input.Get_Value);
--   end Input_Event;
--
--   -----------------------
--   -- Set_Current_Index --
--   -----------------------
--
--   not overriding procedure Set_Current_Index
--    (Self : in out Combo_Box;
--     To   : League.Strings.Universal_String)
--   is
--      use type League.Strings.Universal_String;
--
--      Input : constant WebAPI.HTML.Select_Elements.HTML_Select_Element_Access
--        := WebAPI.HTML.Select_Elements.HTML_Select_Element_Access
--            (Self.Element);
--
--   begin
--      if Input.Get_Value /= To then
--         Input.Set_Value (To);
--         Self.Current_Index_Changed.Emit (Input.Get_Value);
--         --  'input' event is not send when value is updated programmatically.
--      end if;
--   end Set_Current_Index;

   ------------------
   -- Set_Disabled --
   ------------------

   overriding procedure Set_Disabled
    (Self     : in out Abstract_Combo_Box;
     Disabled : Boolean := True)
   is
      Element : Web.HTML.Selects.HTML_Select_Element
        := Self.Element.As_HTML_Select;

   begin
      Element.Set_Disabled (Disabled);
   end Set_Disabled;

   -----------------
   -- Set_Enabled --
   -----------------

   overriding procedure Set_Enabled
    (Self    : in out Abstract_Combo_Box;
     Enabled : Boolean := True) is
   begin
      Self.Set_Disabled (not Enabled);
   end Set_Enabled;

end Web.UI.Widgets.Combo_Boxes;
