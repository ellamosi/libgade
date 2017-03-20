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
