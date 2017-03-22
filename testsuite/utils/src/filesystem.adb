------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body Filesystem is

   function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;
   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   ----------
   -- Join --
   ----------

   function Join
     (Prefix, Suffix           : Pathname;
      Ignore_Absolute_Suffixes : Boolean)
      return Pathname
    is
      use Ada.Strings.Unbounded;

      package String_Vectors is new Ada.Containers.Vectors
        (Positive, Unbounded_String);

      Result : Unbounded_String := +Prefix;
      Names  : String_Vectors.Vector;

   begin
      --  First, decompose Suffix into individual names: the following pushes
      --  the most local directory names first, then the most global ones. For
      --  instance, "a/b/c" will gives: Name => ("c", "b", "a").

      declare
         Suffix_Acc : Unbounded_String := +Suffix;
      begin
         while Length (Suffix_Acc) > 0 loop
            begin
               declare
                  Suffix      : constant String := +Suffix_Acc;
                  Next_Suffix : constant String :=
                    Ada.Directories.Containing_Directory (Suffix);
                  Name        : constant String :=
                    Ada.Directories.Simple_Name (Suffix);
               begin
                  --  The following happens when Suffix is "." (the current
                  --  directory).

                  exit when Suffix = Next_Suffix;
                  Names.Append (+Name);
                  Suffix_Acc := +Next_Suffix;
               end;

            exception
               when Ada.Directories.Use_Error =>

                  --  Suffix is actually an absolute path. Forget about it and
                  --  process it as a relative one.

                  exit when Ignore_Absolute_Suffixes;
                  return Suffix;
            end;
         end loop;
      end;

      --  Then, compose them using Ada.Directories.Compose so we have our
      --  result.

      for Name of reverse Names loop
         Result := +Ada.Directories.Compose (+Result, +Name);
      end loop;
      return +Result;
   end Join;

end Filesystem;
