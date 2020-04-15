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

with Web.Strings;

package Web.UI.Widgets.Labels is

   type Label is new Web.UI.Widgets.Abstract_Widget with private;

   type Label_Access is access all Label'Class
     with Storage_Size => 0;

   procedure Set_Text
    (Self : in out Label'Class;
     To   : Web.Strings.Web_String);

   package Constructors is

      function Create
       (Element : Web.HTML.Elements.HTML_Element'Class)
          return not null Label_Access;

      function Create
       (Id : Web.Strings.Web_String) return not null Label_Access;

      procedure Initialize
       (Self    : in out Label'Class;
        Element : Web.HTML.Elements.HTML_Element'Class);

   end Constructors;

private

   type Label is new Web.UI.Widgets.Abstract_Widget with null record;

   overriding procedure Set_Disabled
    (Self     : in out Label;
     Disabled : Boolean := True);

   overriding procedure Set_Enabled
    (Self    : in out Label;
     Enabled : Boolean := True);

end Web.UI.Widgets.Labels;
