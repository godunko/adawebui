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
--  $Revision: 5720 $ $Date: 2017-01-24 19:41:12 +0300 (Tue, 24 Jan 2017) $
------------------------------------------------------------------------------

with Web.DOM.Documents;
with Web.DOM.Texts;
with Web.Window;

package body Web.UI.Widgets.Labels is

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ------------
      -- Create --
      ------------

      function Create
       (Element : Web.HTML.Elements.HTML_Element'Class)
          return not null Label_Access
      is
         Aux : not null Web.Core.Connectables.Object_Access := new Label;

      begin
         return Result : constant not null Label_Access
           := Label_Access (Aux)
         do
            Initialize (Result.all, Element);
         end return;
      end Create;

      ------------
      -- Create --
      ------------

      function Create
       (Id : Web.Strings.Web_String) return not null Label_Access is
      begin
         return Create (Web.Window.Document.Get_Element_By_Id (Id));
      end Create;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
       (Self    : in out Label'Class;
        Element : Web.HTML.Elements.HTML_Element'Class) is
      begin
         Web.UI.Widgets.Constructors.Initialize (Self, Element);
      end Initialize;

   end Constructors;

   ------------------
   -- Set_Disabled --
   ------------------

   overriding procedure Set_Disabled
    (Self     : in out Label;
     Disabled : Boolean := True) is
   begin
      raise Program_Error;
      --  XXX Not implemented
   end Set_Disabled;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
    (Self : in out Label'Class;
     To   : Web.Strings.Web_String)
   is
      Document : Web.DOM.Documents.Document := Self.Element.Get_Owner_Document;
      Text     : Web.DOM.Texts.Text;

   begin
      --  Remove all children nodes.

      while not Self.Element.Get_First_Child.Is_Null loop
         Self.Element.Remove_Child (Self.Element.Get_First_Child);
      end loop;

      --  Create new text node and insert it.

      Text := Document.Create_Text_Node (To);
      Self.Element.Append_Child (Text);
   end Set_Text;

end Web.UI.Widgets.Labels;
