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

with Web.Window;

package body Web.UI.Widgets.Combo_Boxes.Strings is

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      type String_Combo_Box_Internal_Access is
        access all String_Combo_Box'Class;

      ------------
      -- Create --
      ------------

      function Create
       (Element : Web.HTML.Selects.HTML_Select_Element)
          return not null String_Combo_Box_Access
      is
         Result : constant not null String_Combo_Box_Internal_Access
           := new String_Combo_Box;

      begin
         Initialize (Result.all, Element);

         return String_Combo_Box_Access (Result);
      end Create;

      ------------
      -- Create --
      ------------

      function Create
       (Id : Web.Strings.Web_String) return not null String_Combo_Box_Access is
      begin
         return
           Create (Web.Window.Document.Get_Element_By_Id (Id).As_HTML_Select);
      end Create;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
       (Self    : in out String_Combo_Box'Class;
        Element : Web.HTML.Selects.HTML_Select_Element'Class) is
      begin
         Web.UI.Widgets.Combo_Boxes.Constructors.Initialize (Self, Element);
      end Initialize;

   end Constructors;

   -------------------
   -- Current_Value --
   -------------------

   function Current_Value
    (Self : in out String_Combo_Box'Class) return Web.Strings.Web_String
   is
      Value : constant Web.Strings.Web_String
        := Self.Element.As_HTML_Select.Get_Value;

   begin
      if Value.Is_Empty then
         raise Constraint_Error;

      else
         return Value;
      end if;
   end Current_Value;

--   ----------------------------------
--   -- Current_Index_Changed_Signal --
--   ----------------------------------
--
--   not overriding function Current_Index_Changed_Signal
--    (Self : in out Combo_Box)
--       return not null access WUI.String_Slots.Signal'Class is
--   begin
--      return Self.Current_Index_Changed'Unchecked_Access;
--   end Current_Index_Changed_Signal;
--
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

end Web.UI.Widgets.Combo_Boxes.Strings;
