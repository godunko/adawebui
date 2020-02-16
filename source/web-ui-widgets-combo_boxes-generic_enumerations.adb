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

package body Web.UI.Widgets.Combo_Boxes.Generic_Enumerations is

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ------------
      -- Create --
      ------------

      function Create
       (Element : Web.HTML.Selects.HTML_Select_Element'Class)
          return not null Combo_Box_Access
      is
         Aux : constant not null Web.Core.Connectables.Object_Access
           := new Combo_Box;

      begin
         return Result : constant not null Combo_Box_Access
           := Combo_Box_Access (Aux)
         do
            Initialize (Result.all, Element);
         end return;
      end Create;

      ------------
      -- Create --
      ------------

      function Create
       (Id : Web.Strings.Web_String) return not null Combo_Box_Access is
      begin
         return
           Create (Web.Window.Document.Get_Element_By_Id (Id).As_HTML_Select);
      end Create;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
       (Self    : in out Combo_Box'Class;
        Element : Web.HTML.Selects.HTML_Select_Element'Class) is
      begin
         Web.UI.Widgets.Combo_Boxes.Constructors.Initialize (Self, Element);
      end Initialize;

   end Constructors;

   -------------------
   -- Current_Value --
   -------------------

   function Current_Value (Self : in out Combo_Box'Class) return Data_Type is
      Value : constant Web.Strings.Web_String
        := Combo_Box (Self).Element.As_HTML_Select.Get_Value;
      --  XXX GNATLLVM: explicit type convention is necessary to workaround
      --  crash of the compiler.

   begin
      if Value.Is_Empty then
         raise Constraint_Error;

      else
         return Data_Type'Wide_Wide_Value (Value.To_Wide_Wide_String);
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

end Web.UI.Widgets.Combo_Boxes.Generic_Enumerations;
