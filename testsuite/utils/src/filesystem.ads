package Filesystem is

   subtype Pathname is String;

   -------------
   -- Helpers --
   -------------

   function Join
     (Prefix, Suffix           : Pathname;
      Ignore_Absolute_Suffixes : Boolean)
      return Pathname;
   --  Like Ada.Directories.Compose, but also accepts a full path as Suffix.
   --  For instance:
   --
   --     Join ("/a", "b")   => "/a/b"
   --     Join ("/a", "b/c") => "/a/b/c"
   --
   --  If Ignore_Absolute_Suffixes is True, if Suffix is an absolute path, do
   --  as if it was a relative path instead:
   --
   --     Join ("/a", "/b")  => "/a/b"
   --
   --  Otherwise, if sufifx is an absolute path, just return Suffix.

end Filesystem;
