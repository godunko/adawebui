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
--  $Revision: 5733 $ $Date: 2017-01-28 14:53:14 +0300 (Sat, 28 Jan 2017) $
------------------------------------------------------------------------------

with Web.HTML.Validity_States;
with Web.Window;

package body Web.UI.Widgets.Spin_Boxes.Generic_Floats is

   procedure Internal_Set_Value
--    (Self   : in out Float_Spin_Box'Class;
--  XXX GNATLLVM: compiler crash
    (Self   : in out Float_Spin_Box;
     To     : Data_Type;
     Update : Boolean);
   --  Sets value, emit signal when value was modified, and update value of
   --  input element when Update is True.

   function Image (Item : Data_Type) return Web.Strings.Web_String;

   ------------------
   -- Change_Event --
   ------------------

   overriding procedure Change_Event (Self : in out Float_Spin_Box) is
      Input    : constant Web.HTML.Inputs.HTML_Input_Element
        := Self.Element.As_HTML_Input;
      Validity : constant Web.HTML.Validity_States.Validity_State
        := Input.Get_Validity;

   begin
      if not Validity.Get_Valid then
         if Validity.Get_Bad_Input then
            Self.Internal_Set_Value
             (Data_Type'Wide_Wide_Value (Input.Get_Min.To_Wide_Wide_String),
              True);

         elsif Validity.Get_Range_Overflow then
            Self.Internal_Set_Value
             (Data_Type'Wide_Wide_Value (Input.Get_Max.To_Wide_Wide_String),
              True);

         elsif Validity.Get_Range_Underflow then
            Self.Internal_Set_Value
             (Data_Type'Wide_Wide_Value (Input.Get_Min.To_Wide_Wide_String),
              True);

         elsif Validity.Get_Value_Missing then
            Self.Internal_Set_Value
             (Data_Type'Wide_Wide_Value (Input.Get_Min.To_Wide_Wide_String),
              True);
         end if;
      end if;

--      Self.Editing_Finished.Emit;
   end Change_Event;

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      type Float_Spin_Box_Internal_Access is access all Float_Spin_Box'Class;

      ------------
      -- Create --
      ------------

      function Create
       (Element : Web.HTML.Inputs.HTML_Input_Element'Class)
          return not null Float_Spin_Box_Access
      is
         Aux : constant not null Float_Spin_Box_Internal_Access
           := new Float_Spin_Box;

      begin
         return Result : constant not null Float_Spin_Box_Access
           := Float_Spin_Box_Access (Aux)
         do
            Initialize (Result.all, Element);
         end return;
      end Create;

      ------------
      -- Create --
      ------------

      function Create
       (Id : Web.Strings.Web_String) return not null Float_Spin_Box_Access is
      begin
         return
           Create (Web.Window.Document.Get_Element_By_Id (Id).As_HTML_Input);
      end Create;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
       (Self    : in out Float_Spin_Box'Class;
        Element : Web.HTML.Inputs.HTML_Input_Element'Class) is
      begin
         Web.UI.Widgets.Spin_Boxes.Constructors.Initialize (Self, Element);

         --  Extract properties value from HTML element.

         Self.Last_Value :=
           Data_Type'Wide_Wide_Value
            (Element.Get_Value.To_Wide_Wide_String);
      end Initialize;

   end Constructors;

   -----------
   -- Value --
   -----------

   function Get_Value (Self : Float_Spin_Box'Class) return Data_Type is
   begin
      return Self.Last_Value;
   end Get_Value;

   -----------
   -- Image --
   -----------

   function Image (Item : Data_Type) return Web.Strings.Web_String is
      Aux   : constant Wide_Wide_String := Data_Type'Wide_Wide_Image (Item);
      Image : constant Wide_Wide_String
        := Aux
            ((if Aux (Aux'First) = ' ' then Aux'First + 1 else Aux'First)
               .. Aux'Last);

   begin
      --  XXX Need to be rewritten to avoid use of exponential form when
      --  possible. Most probably it should use configuration of underlying
      --  'input' element.

      return +Image;
   end Image;

   -----------------
   -- Input_Event --
   -----------------

   overriding procedure Input_Event (Self  : in out Float_Spin_Box) is
      Input : constant Web.HTML.Inputs.HTML_Input_Element
        := Self.Element.As_HTML_Input;

   begin
      if Input.Get_Validity.Get_Valid then
         Self.Last_Value :=
           Data_Type'Wide_Wide_Value (Input.Get_Value.To_Wide_Wide_String);
         Self.Value_Changed.Emit (Self.Last_Value);
      end if;
   end Input_Event;

   ------------------------
   -- Internal_Set_Value --
   ------------------------

   procedure Internal_Set_Value
--    (Self   : in out Float_Spin_Box'Class;
--  XXX GNATLLVM: compiler crash
    (Self   : in out Float_Spin_Box;
     To     : Data_Type;
     Update : Boolean)
   is
      Input : Web.HTML.Inputs.HTML_Input_Element
        := Self.Element.As_HTML_Input;

   begin
      if Self.Last_Value /= To then
         Self.Last_Value := To;
         Input.Set_Value (Image (To));
         Self.Value_Changed.Emit (To);
         --  'input' event is not send when value is changed programmatically.

      elsif Update then
         Input.Set_Value (Image (To));
      end if;
   end Internal_Set_Value;

--   ---------------
--   -- Set_Value --
--   ---------------
--
--   not overriding procedure Set_Value
--    (Self : in out Float_Spin_Box;
--     To   : Data_Type) is
--   begin
--      Self.Internal_Set_Value (To, False);
--   end Set_Value;
--
--   ---------------
--   -- Step_Down --
--   ---------------
--
--   overriding procedure Step_Down (Self : in out Float_Spin_Box) is
--   begin
--      raise Program_Error;
--   end Step_Down;
--
--   -------------
--   -- Step_Up --
--   -------------
--
--   overriding procedure Step_Up (Self : in out Float_Spin_Box) is
--   begin
--      raise Program_Error;
--   end Step_Up;

   --------------------------
   -- Value_Changed_Signal --
   --------------------------

   function Value_Changed_Signal
    (Self : in out Float_Spin_Box'Class)
       return not null access Float_Slots.Signal'Class is
   begin
      return Self.Value_Changed'Unchecked_Access;
   end Value_Changed_Signal;

end Web.UI.Widgets.Spin_Boxes.Generic_Floats;
