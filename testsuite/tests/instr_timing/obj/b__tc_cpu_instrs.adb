pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__tc_cpu_instrs.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__tc_cpu_instrs.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exception_table_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exceptions_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__secondary_stack_E");
   E260 : Short_Integer; pragma Import (Ada, E260, "ada__containers_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "ada__io_exceptions_E");
   E234 : Short_Integer; pragma Import (Ada, E234, "ada__strings_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "interfaces__c_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "system__os_lib_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__tags_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__streams_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "system__file_control_block_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "system__finalization_root_E");
   E076 : Short_Integer; pragma Import (Ada, E076, "ada__finalization_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "system__file_io_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__streams__stream_io_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "system__storage_pools_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "system__finalization_masters_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "system__storage_pools__subpools_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "ada__calendar_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "ada__calendar__time_zones_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "ada__text_io_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "gnat__secure_hashes_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "gnat__secure_hashes__md5_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "gnat__md5_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "system__assertions_E");
   E236 : Short_Integer; pragma Import (Ada, E236, "ada__strings__maps_E");
   E239 : Short_Integer; pragma Import (Ada, E239, "ada__strings__maps__constants_E");
   E247 : Short_Integer; pragma Import (Ada, E247, "ada__strings__unbounded_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "system__pool_global_E");
   E257 : Short_Integer; pragma Import (Ada, E257, "system__regexp_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "ada__directories_E");
   E147 : Short_Integer; pragma Import (Ada, E147, "system__sequential_io_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "bitmap__buffer_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "bitmap__color_conversion_E");
   E107 : Short_Integer; pragma Import (Ada, E107, "bitmap__soft_drawing_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "bitmap__memory_mapped_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "bitmap__file_io_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "compare_files_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "filesystem_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "gade__dev_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "gade__dev__cpu_E");
   E200 : Short_Integer; pragma Import (Ada, E200, "gade__dev__cpu__arithmetic_E");
   E202 : Short_Integer; pragma Import (Ada, E202, "gade__dev__cpu__bitwise_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "gade__dev__cpu__logic_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "gade__dev__external_ram_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "gade__input_reader_E");
   E172 : Short_Integer; pragma Import (Ada, E172, "gade__dev__joypad_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "gade__dev__oam_E");
   E162 : Short_Integer; pragma Import (Ada, E162, "gade__dev__video__tile_buffer_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "gade__dev__video__tile_map_E");
   E160 : Short_Integer; pragma Import (Ada, E160, "gade__dev__vram_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "gade__dev__video__background_buffer_E");
   E158 : Short_Integer; pragma Import (Ada, E158, "gade__dev__video__sprites_E");
   E168 : Short_Integer; pragma Import (Ada, E168, "gade__dev__video__window_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "gade__dev__cartridge_E");
   E174 : Short_Integer; pragma Import (Ada, E174, "gade__dev__timer_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "gade__dev__interrupts_E");
   E151 : Short_Integer; pragma Import (Ada, E151, "gade__dev__display_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "gade__gb_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "gade__gb__memory_map_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "gade__dev__cpu__instructions_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "gade__dev__cpu__instructions__exec_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "gade__interfaces_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "test_directories_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "test_lcd_output_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E135 := E135 - 1;
      E137 := E137 - 1;
      E151 := E151 - 1;
      E153 := E153 - 1;
      E174 := E174 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "gade__gb__finalize_spec");
      begin
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "gade__dev__display__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "gade__dev__interrupts__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "gade__dev__timer__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "gade__dev__cartridge__finalize_spec");
      begin
         F5;
      end;
      E160 := E160 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "gade__dev__vram__finalize_spec");
      begin
         F6;
      end;
      E166 := E166 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "gade__dev__oam__finalize_spec");
      begin
         F7;
      end;
      E172 := E172 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gade__dev__joypad__finalize_spec");
      begin
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gade__input_reader__finalize_spec");
      begin
         E129 := E129 - 1;
         F9;
      end;
      E139 := E139 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "gade__dev__external_ram__finalize_spec");
      begin
         F10;
      end;
      E149 := E149 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "gade__dev__cpu__finalize_spec");
      begin
         F11;
      end;
      E103 := E103 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "bitmap__memory_mapped__finalize_spec");
      begin
         F12;
      end;
      E107 := E107 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "bitmap__soft_drawing__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "bitmap__buffer__finalize_spec");
      begin
         E087 := E087 - 1;
         F14;
      end;
      E147 := E147 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "system__sequential_io__finalize_spec");
      begin
         F15;
      end;
      E212 := E212 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "ada__directories__finalize_spec");
      begin
         F16;
      end;
      E257 := E257 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "system__regexp__finalize_spec");
      begin
         F17;
      end;
      E097 := E097 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "system__pool_global__finalize_spec");
      begin
         F18;
      end;
      E247 := E247 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "ada__strings__unbounded__finalize_spec");
      begin
         F19;
      end;
      E085 := E085 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "ada__text_io__finalize_spec");
      begin
         F20;
      end;
      E113 := E113 - 1;
      declare
         procedure F21;
         pragma Import (Ada, F21, "system__storage_pools__subpools__finalize_spec");
      begin
         F21;
      end;
      E089 := E089 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "system__finalization_masters__finalize_spec");
      begin
         F22;
      end;
      E068 := E068 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "ada__streams__stream_io__finalize_spec");
      begin
         F23;
      end;
      declare
         procedure F24;
         pragma Import (Ada, F24, "system__file_io__finalize_body");
      begin
         E075 := E075 - 1;
         F24;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E025 := E025 + 1;
      System.Exceptions'Elab_Spec;
      E027 := E027 + 1;
      System.Soft_Links'Elab_Body;
      E015 := E015 + 1;
      System.Secondary_Stack'Elab_Body;
      E019 := E019 + 1;
      Ada.Containers'Elab_Spec;
      E260 := E260 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E053 := E053 + 1;
      Ada.Strings'Elab_Spec;
      E234 := E234 + 1;
      Interfaces.C'Elab_Spec;
      E216 := E216 + 1;
      System.Os_Lib'Elab_Body;
      E080 := E080 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E055 := E055 + 1;
      Ada.Streams'Elab_Spec;
      E008 := E008 + 1;
      System.File_Control_Block'Elab_Spec;
      E083 := E083 + 1;
      System.Finalization_Root'Elab_Spec;
      E078 := E078 + 1;
      Ada.Finalization'Elab_Spec;
      E076 := E076 + 1;
      System.File_Io'Elab_Body;
      E075 := E075 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E068 := E068 + 1;
      System.Storage_Pools'Elab_Spec;
      E095 := E095 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E089 := E089 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E113 := E113 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E214 := E214 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E222 := E222 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E085 := E085 + 1;
      E122 := E122 + 1;
      E124 := E124 + 1;
      Gnat.Md5'Elab_Spec;
      E120 := E120 + 1;
      System.Assertions'Elab_Spec;
      E109 := E109 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E236 := E236 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E239 := E239 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E247 := E247 + 1;
      System.Pool_Global'Elab_Spec;
      E097 := E097 + 1;
      System.Regexp'Elab_Spec;
      E257 := E257 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E212 := E212 + 1;
      System.Sequential_Io'Elab_Spec;
      E147 := E147 + 1;
      Bitmap.Buffer'Elab_Spec;
      E087 := E087 + 1;
      E105 := E105 + 1;
      Bitmap.Soft_Drawing'Elab_Spec;
      Bitmap.Soft_Drawing'Elab_Body;
      E107 := E107 + 1;
      Bitmap.Memory_Mapped'Elab_Spec;
      Bitmap.Memory_Mapped'Elab_Body;
      E103 := E103 + 1;
      E101 := E101 + 1;
      E117 := E117 + 1;
      E259 := E259 + 1;
      Gade.Dev'Elab_Spec;
      E133 := E133 + 1;
      Gade.Dev.Cpu'Elab_Spec;
      Gade.Dev.Cpu'Elab_Body;
      E149 := E149 + 1;
      E200 := E200 + 1;
      E202 := E202 + 1;
      E204 := E204 + 1;
      Gade.Dev.External_Ram'Elab_Spec;
      Gade.Dev.External_Ram'Elab_Body;
      E139 := E139 + 1;
      Gade.Input_Reader'Elab_Spec;
      E129 := E129 + 1;
      Gade.Dev.Joypad'Elab_Spec;
      Gade.Dev.Joypad'Elab_Body;
      E172 := E172 + 1;
      Gade.Dev.Oam'Elab_Spec;
      Gade.Dev.Oam'Elab_Body;
      E166 := E166 + 1;
      E164 := E164 + 1;
      Gade.Dev.Vram'Elab_Spec;
      Gade.Dev.Vram'Elab_Body;
      E160 := E160 + 1;
      E162 := E162 + 1;
      E170 := E170 + 1;
      Gade.Dev.Video.Sprites'Elab_Spec;
      E158 := E158 + 1;
      E168 := E168 + 1;
      Gade.Dev.Cartridge'Elab_Spec;
      Gade.Dev.Timer'Elab_Spec;
      Gade.Dev.Interrupts'Elab_Spec;
      Gade.Dev.Display'Elab_Spec;
      Gade.Gb'Elab_Spec;
      Gade.Dev.Timer'Elab_Body;
      E174 := E174 + 1;
      E155 := E155 + 1;
      Gade.Dev.Interrupts'Elab_Body;
      E153 := E153 + 1;
      Gade.Dev.Display'Elab_Body;
      E151 := E151 + 1;
      Gade.Dev.Cartridge'Elab_Body;
      E137 := E137 + 1;
      Gade.Gb'Elab_Body;
      E135 := E135 + 1;
      E176 := E176 + 1;
      E206 := E206 + 1;
      Gade.Interfaces'Elab_Spec;
      E132 := E132 + 1;
      Test_Directories'Elab_Spec;
      E208 := E208 + 1;
      E005 := E005 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_tc_cpu_instrs");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/compare_files.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/filesystem.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-CPU.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-CPU-Arithmetic.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-CPU-Bitwise.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-CPU-Logic.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-External_RAM.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-Video.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Input_Reader.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-Joypad.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Video_Buffer.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-OAM.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-Video-Tile_Map.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-VRAM.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-Video-Tile_Buffer.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-Video-Background_Buffer.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-Video-Sprites.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-Video-Window.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-Timer.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-GB-Memory_Map.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-Interrupts.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-Display.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-Cartridge.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-GB.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-CPU-Instructions.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-CPU-Instructions-Table.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Dev-CPU-Instructions-Exec.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/Gade-Interfaces.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/test_directories.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/test_lcd_output.o
   --   /Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/tc_cpu_instrs.o
   --   -L/Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/
   --   -L/Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/tests/cpu_instrs/obj/
   --   -L/Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/utils/obj/
   --   -L/Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/obj/libgade/core/debug/
   --   -L/Users/ellamosi/Dropbox (Personal)/Exchange/GameBoy/libgade/testsuite/utils/libraries/bmp/lib/Debug/
   --   -L/usr/local/gnat/lib/gcc/x86_64-apple-darwin14.5.0/6.3.1/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
