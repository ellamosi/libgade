pragma Warnings (Off);
pragma Ada_95;
with System;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2017 (20170515-63)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_tc_instr_timing" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#4bad09d0#;
   pragma Export (C, u00001, "tc_instr_timingB");
   u00002 : constant Version_32 := 16#b6df930e#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#30ae102c#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#2aa5e292#;
   pragma Export (C, u00004, "test_lcd_outputB");
   u00005 : constant Version_32 := 16#42538193#;
   pragma Export (C, u00005, "test_lcd_outputS");
   u00006 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00006, "adaS");
   u00007 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00007, "ada__streamsB");
   u00008 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00008, "ada__streamsS");
   u00009 : constant Version_32 := 16#ccb4b432#;
   pragma Export (C, u00009, "ada__exceptionsB");
   u00010 : constant Version_32 := 16#20f622c0#;
   pragma Export (C, u00010, "ada__exceptionsS");
   u00011 : constant Version_32 := 16#e947e6a9#;
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerB");
   u00012 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00012, "ada__exceptions__last_chance_handlerS");
   u00013 : constant Version_32 := 16#085b6ffb#;
   pragma Export (C, u00013, "systemS");
   u00014 : constant Version_32 := 16#4e7785b8#;
   pragma Export (C, u00014, "system__soft_linksB");
   u00015 : constant Version_32 := 16#96dfb7ae#;
   pragma Export (C, u00015, "system__soft_linksS");
   u00016 : constant Version_32 := 16#b01dad17#;
   pragma Export (C, u00016, "system__parametersB");
   u00017 : constant Version_32 := 16#76716284#;
   pragma Export (C, u00017, "system__parametersS");
   u00018 : constant Version_32 := 16#30ad09e5#;
   pragma Export (C, u00018, "system__secondary_stackB");
   u00019 : constant Version_32 := 16#b2c99081#;
   pragma Export (C, u00019, "system__secondary_stackS");
   u00020 : constant Version_32 := 16#f103f468#;
   pragma Export (C, u00020, "system__storage_elementsB");
   u00021 : constant Version_32 := 16#259825ff#;
   pragma Export (C, u00021, "system__storage_elementsS");
   u00022 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00022, "system__stack_checkingB");
   u00023 : constant Version_32 := 16#86e40413#;
   pragma Export (C, u00023, "system__stack_checkingS");
   u00024 : constant Version_32 := 16#87a448ff#;
   pragma Export (C, u00024, "system__exception_tableB");
   u00025 : constant Version_32 := 16#55f506b9#;
   pragma Export (C, u00025, "system__exception_tableS");
   u00026 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00026, "system__exceptionsB");
   u00027 : constant Version_32 := 16#6038020d#;
   pragma Export (C, u00027, "system__exceptionsS");
   u00028 : constant Version_32 := 16#80916427#;
   pragma Export (C, u00028, "system__exceptions__machineB");
   u00029 : constant Version_32 := 16#047ef179#;
   pragma Export (C, u00029, "system__exceptions__machineS");
   u00030 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00030, "system__exceptions_debugB");
   u00031 : constant Version_32 := 16#76d1963f#;
   pragma Export (C, u00031, "system__exceptions_debugS");
   u00032 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00032, "system__img_intB");
   u00033 : constant Version_32 := 16#0a808f39#;
   pragma Export (C, u00033, "system__img_intS");
   u00034 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00034, "system__tracebackB");
   u00035 : constant Version_32 := 16#5679b13f#;
   pragma Export (C, u00035, "system__tracebackS");
   u00036 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00036, "system__traceback_entriesB");
   u00037 : constant Version_32 := 16#0800998b#;
   pragma Export (C, u00037, "system__traceback_entriesS");
   u00038 : constant Version_32 := 16#2e33df74#;
   pragma Export (C, u00038, "system__traceback__symbolicB");
   u00039 : constant Version_32 := 16#9df1ae6d#;
   pragma Export (C, u00039, "system__traceback__symbolicS");
   u00040 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00040, "ada__exceptions__tracebackB");
   u00041 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00041, "ada__exceptions__tracebackS");
   u00042 : constant Version_32 := 16#9f00b3d3#;
   pragma Export (C, u00042, "system__address_imageB");
   u00043 : constant Version_32 := 16#a9b7f2c1#;
   pragma Export (C, u00043, "system__address_imageS");
   u00044 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00044, "system__wch_conB");
   u00045 : constant Version_32 := 16#13264d29#;
   pragma Export (C, u00045, "system__wch_conS");
   u00046 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00046, "system__wch_stwB");
   u00047 : constant Version_32 := 16#3e376128#;
   pragma Export (C, u00047, "system__wch_stwS");
   u00048 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00048, "system__wch_cnvB");
   u00049 : constant Version_32 := 16#1c91f7da#;
   pragma Export (C, u00049, "system__wch_cnvS");
   u00050 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00050, "interfacesS");
   u00051 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00051, "system__wch_jisB");
   u00052 : constant Version_32 := 16#9ce1eefb#;
   pragma Export (C, u00052, "system__wch_jisS");
   u00053 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00053, "ada__io_exceptionsS");
   u00054 : constant Version_32 := 16#d85792d6#;
   pragma Export (C, u00054, "ada__tagsB");
   u00055 : constant Version_32 := 16#8813468c#;
   pragma Export (C, u00055, "ada__tagsS");
   u00056 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00056, "system__htableB");
   u00057 : constant Version_32 := 16#8c99dc11#;
   pragma Export (C, u00057, "system__htableS");
   u00058 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00058, "system__string_hashB");
   u00059 : constant Version_32 := 16#2ec7b76f#;
   pragma Export (C, u00059, "system__string_hashS");
   u00060 : constant Version_32 := 16#3cdd1378#;
   pragma Export (C, u00060, "system__unsigned_typesS");
   u00061 : constant Version_32 := 16#afdbf393#;
   pragma Export (C, u00061, "system__val_lluB");
   u00062 : constant Version_32 := 16#462f440a#;
   pragma Export (C, u00062, "system__val_lluS");
   u00063 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00063, "system__val_utilB");
   u00064 : constant Version_32 := 16#a4fbd905#;
   pragma Export (C, u00064, "system__val_utilS");
   u00065 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00065, "system__case_utilB");
   u00066 : constant Version_32 := 16#2c52062c#;
   pragma Export (C, u00066, "system__case_utilS");
   u00067 : constant Version_32 := 16#2dbaf09b#;
   pragma Export (C, u00067, "ada__streams__stream_ioB");
   u00068 : constant Version_32 := 16#31fc8e02#;
   pragma Export (C, u00068, "ada__streams__stream_ioS");
   u00069 : constant Version_32 := 16#4c01b69c#;
   pragma Export (C, u00069, "interfaces__c_streamsB");
   u00070 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00070, "interfaces__c_streamsS");
   u00071 : constant Version_32 := 16#78cab9f5#;
   pragma Export (C, u00071, "system__crtlS");
   u00072 : constant Version_32 := 16#5de653db#;
   pragma Export (C, u00072, "system__communicationB");
   u00073 : constant Version_32 := 16#113b3a29#;
   pragma Export (C, u00073, "system__communicationS");
   u00074 : constant Version_32 := 16#6f0d52aa#;
   pragma Export (C, u00074, "system__file_ioB");
   u00075 : constant Version_32 := 16#af2a8e9e#;
   pragma Export (C, u00075, "system__file_ioS");
   u00076 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00076, "ada__finalizationS");
   u00077 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00077, "system__finalization_rootB");
   u00078 : constant Version_32 := 16#47a91c6b#;
   pragma Export (C, u00078, "system__finalization_rootS");
   u00079 : constant Version_32 := 16#6e98c0bf#;
   pragma Export (C, u00079, "system__os_libB");
   u00080 : constant Version_32 := 16#ed466fde#;
   pragma Export (C, u00080, "system__os_libS");
   u00081 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00081, "system__stringsB");
   u00082 : constant Version_32 := 16#76e47e9d#;
   pragma Export (C, u00082, "system__stringsS");
   u00083 : constant Version_32 := 16#f5c4f553#;
   pragma Export (C, u00083, "system__file_control_blockS");
   u00084 : constant Version_32 := 16#1d1c6062#;
   pragma Export (C, u00084, "ada__text_ioB");
   u00085 : constant Version_32 := 16#af8af06f#;
   pragma Export (C, u00085, "ada__text_ioS");
   u00086 : constant Version_32 := 16#ef6873df#;
   pragma Export (C, u00086, "bitmapS");
   u00087 : constant Version_32 := 16#4be48347#;
   pragma Export (C, u00087, "bitmap__bufferS");
   u00088 : constant Version_32 := 16#6abe5dbe#;
   pragma Export (C, u00088, "system__finalization_mastersB");
   u00089 : constant Version_32 := 16#53a75631#;
   pragma Export (C, u00089, "system__finalization_mastersS");
   u00090 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00090, "system__img_boolB");
   u00091 : constant Version_32 := 16#fd821e10#;
   pragma Export (C, u00091, "system__img_boolS");
   u00092 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00092, "system__ioB");
   u00093 : constant Version_32 := 16#961998b4#;
   pragma Export (C, u00093, "system__ioS");
   u00094 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00094, "system__storage_poolsB");
   u00095 : constant Version_32 := 16#2bb6f156#;
   pragma Export (C, u00095, "system__storage_poolsS");
   u00096 : constant Version_32 := 16#5a895de2#;
   pragma Export (C, u00096, "system__pool_globalB");
   u00097 : constant Version_32 := 16#7141203e#;
   pragma Export (C, u00097, "system__pool_globalS");
   u00098 : constant Version_32 := 16#a6359005#;
   pragma Export (C, u00098, "system__memoryB");
   u00099 : constant Version_32 := 16#512609cf#;
   pragma Export (C, u00099, "system__memoryS");
   u00100 : constant Version_32 := 16#d5da86e3#;
   pragma Export (C, u00100, "bitmap__file_ioB");
   u00101 : constant Version_32 := 16#126a5537#;
   pragma Export (C, u00101, "bitmap__file_ioS");
   u00102 : constant Version_32 := 16#a97974c0#;
   pragma Export (C, u00102, "bitmap__memory_mappedB");
   u00103 : constant Version_32 := 16#b0086216#;
   pragma Export (C, u00103, "bitmap__memory_mappedS");
   u00104 : constant Version_32 := 16#eae689a7#;
   pragma Export (C, u00104, "bitmap__color_conversionB");
   u00105 : constant Version_32 := 16#cc732bce#;
   pragma Export (C, u00105, "bitmap__color_conversionS");
   u00106 : constant Version_32 := 16#80b31487#;
   pragma Export (C, u00106, "bitmap__soft_drawingB");
   u00107 : constant Version_32 := 16#24240c6f#;
   pragma Export (C, u00107, "bitmap__soft_drawingS");
   u00108 : constant Version_32 := 16#52f1910f#;
   pragma Export (C, u00108, "system__assertionsB");
   u00109 : constant Version_32 := 16#c5d6436f#;
   pragma Export (C, u00109, "system__assertionsS");
   u00110 : constant Version_32 := 16#3c420900#;
   pragma Export (C, u00110, "system__stream_attributesB");
   u00111 : constant Version_32 := 16#8bc30a4e#;
   pragma Export (C, u00111, "system__stream_attributesS");
   u00112 : constant Version_32 := 16#a2250034#;
   pragma Export (C, u00112, "system__storage_pools__subpoolsB");
   u00113 : constant Version_32 := 16#cc5a1856#;
   pragma Export (C, u00113, "system__storage_pools__subpoolsS");
   u00114 : constant Version_32 := 16#9aad1ff1#;
   pragma Export (C, u00114, "system__storage_pools__subpools__finalizationB");
   u00115 : constant Version_32 := 16#fe2f4b3a#;
   pragma Export (C, u00115, "system__storage_pools__subpools__finalizationS");
   u00116 : constant Version_32 := 16#d4cd7a61#;
   pragma Export (C, u00116, "compare_filesB");
   u00117 : constant Version_32 := 16#991aaced#;
   pragma Export (C, u00117, "compare_filesS");
   u00118 : constant Version_32 := 16#fd2ad2f1#;
   pragma Export (C, u00118, "gnatS");
   u00119 : constant Version_32 := 16#bb55398e#;
   pragma Export (C, u00119, "gnat__md5B");
   u00120 : constant Version_32 := 16#3b049100#;
   pragma Export (C, u00120, "gnat__md5S");
   u00121 : constant Version_32 := 16#873bf502#;
   pragma Export (C, u00121, "gnat__secure_hashesB");
   u00122 : constant Version_32 := 16#8503c95f#;
   pragma Export (C, u00122, "gnat__secure_hashesS");
   u00123 : constant Version_32 := 16#462993a2#;
   pragma Export (C, u00123, "gnat__secure_hashes__md5B");
   u00124 : constant Version_32 := 16#b01bf85b#;
   pragma Export (C, u00124, "gnat__secure_hashes__md5S");
   u00125 : constant Version_32 := 16#8d4c13dd#;
   pragma Export (C, u00125, "gnat__byte_swappingB");
   u00126 : constant Version_32 := 16#51a3ab33#;
   pragma Export (C, u00126, "gnat__byte_swappingS");
   u00127 : constant Version_32 := 16#71c71bbb#;
   pragma Export (C, u00127, "system__byte_swappingS");
   u00128 : constant Version_32 := 16#743553e8#;
   pragma Export (C, u00128, "gadeS");
   u00129 : constant Version_32 := 16#6295cc11#;
   pragma Export (C, u00129, "gade__input_readerS");
   u00130 : constant Version_32 := 16#eea4be54#;
   pragma Export (C, u00130, "gade__video_bufferS");
   u00131 : constant Version_32 := 16#a6379022#;
   pragma Export (C, u00131, "gade__interfacesB");
   u00132 : constant Version_32 := 16#43d2f6dd#;
   pragma Export (C, u00132, "gade__interfacesS");
   u00133 : constant Version_32 := 16#e1588d9a#;
   pragma Export (C, u00133, "gade__devS");
   u00134 : constant Version_32 := 16#a4b79901#;
   pragma Export (C, u00134, "gade__gbB");
   u00135 : constant Version_32 := 16#110ed863#;
   pragma Export (C, u00135, "gade__gbS");
   u00136 : constant Version_32 := 16#37b19786#;
   pragma Export (C, u00136, "gade__dev__cartridgeB");
   u00137 : constant Version_32 := 16#bd442424#;
   pragma Export (C, u00137, "gade__dev__cartridgeS");
   u00138 : constant Version_32 := 16#c8a04f29#;
   pragma Export (C, u00138, "gade__dev__external_ramB");
   u00139 : constant Version_32 := 16#582d46bd#;
   pragma Export (C, u00139, "gade__dev__external_ramS");
   u00140 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00140, "system__concat_2B");
   u00141 : constant Version_32 := 16#0afbb82b#;
   pragma Export (C, u00141, "system__concat_2S");
   u00142 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00142, "system__concat_3B");
   u00143 : constant Version_32 := 16#032b335e#;
   pragma Export (C, u00143, "system__concat_3S");
   u00144 : constant Version_32 := 16#18e0e51c#;
   pragma Export (C, u00144, "system__img_enum_newB");
   u00145 : constant Version_32 := 16#6917693b#;
   pragma Export (C, u00145, "system__img_enum_newS");
   u00146 : constant Version_32 := 16#796b5f0d#;
   pragma Export (C, u00146, "system__sequential_ioB");
   u00147 : constant Version_32 := 16#e237c50b#;
   pragma Export (C, u00147, "system__sequential_ioS");
   u00148 : constant Version_32 := 16#35d3392e#;
   pragma Export (C, u00148, "gade__dev__cpuB");
   u00149 : constant Version_32 := 16#2216d7fc#;
   pragma Export (C, u00149, "gade__dev__cpuS");
   u00150 : constant Version_32 := 16#4c99fdf9#;
   pragma Export (C, u00150, "gade__dev__displayB");
   u00151 : constant Version_32 := 16#4b14a37a#;
   pragma Export (C, u00151, "gade__dev__displayS");
   u00152 : constant Version_32 := 16#3e2d1d28#;
   pragma Export (C, u00152, "gade__dev__interruptsB");
   u00153 : constant Version_32 := 16#363a9b1b#;
   pragma Export (C, u00153, "gade__dev__interruptsS");
   u00154 : constant Version_32 := 16#99108ab9#;
   pragma Export (C, u00154, "gade__gb__memory_mapB");
   u00155 : constant Version_32 := 16#c1c51f35#;
   pragma Export (C, u00155, "gade__gb__memory_mapS");
   u00156 : constant Version_32 := 16#743ec10d#;
   pragma Export (C, u00156, "gade__dev__videoS");
   u00157 : constant Version_32 := 16#6ac3b0cc#;
   pragma Export (C, u00157, "gade__dev__video__spritesB");
   u00158 : constant Version_32 := 16#f4f6404e#;
   pragma Export (C, u00158, "gade__dev__video__spritesS");
   u00159 : constant Version_32 := 16#db35986d#;
   pragma Export (C, u00159, "gade__dev__vramB");
   u00160 : constant Version_32 := 16#8e01eb95#;
   pragma Export (C, u00160, "gade__dev__vramS");
   u00161 : constant Version_32 := 16#468e246b#;
   pragma Export (C, u00161, "gade__dev__video__tile_bufferB");
   u00162 : constant Version_32 := 16#296dff00#;
   pragma Export (C, u00162, "gade__dev__video__tile_bufferS");
   u00163 : constant Version_32 := 16#331ce4a5#;
   pragma Export (C, u00163, "gade__dev__video__tile_mapB");
   u00164 : constant Version_32 := 16#15b4761f#;
   pragma Export (C, u00164, "gade__dev__video__tile_mapS");
   u00165 : constant Version_32 := 16#e62fbd23#;
   pragma Export (C, u00165, "gade__dev__oamB");
   u00166 : constant Version_32 := 16#0b81045a#;
   pragma Export (C, u00166, "gade__dev__oamS");
   u00167 : constant Version_32 := 16#9fd2fe88#;
   pragma Export (C, u00167, "gade__dev__video__windowB");
   u00168 : constant Version_32 := 16#87c5f825#;
   pragma Export (C, u00168, "gade__dev__video__windowS");
   u00169 : constant Version_32 := 16#8fad3dc2#;
   pragma Export (C, u00169, "gade__dev__video__background_bufferB");
   u00170 : constant Version_32 := 16#931b2701#;
   pragma Export (C, u00170, "gade__dev__video__background_bufferS");
   u00171 : constant Version_32 := 16#a1404277#;
   pragma Export (C, u00171, "gade__dev__joypadB");
   u00172 : constant Version_32 := 16#259ab45c#;
   pragma Export (C, u00172, "gade__dev__joypadS");
   u00173 : constant Version_32 := 16#7f2eeb3e#;
   pragma Export (C, u00173, "gade__dev__timerB");
   u00174 : constant Version_32 := 16#9ca3f3f8#;
   pragma Export (C, u00174, "gade__dev__timerS");
   u00175 : constant Version_32 := 16#ae76ba66#;
   pragma Export (C, u00175, "gade__dev__cpu__instructionsB");
   u00176 : constant Version_32 := 16#b49fa2eb#;
   pragma Export (C, u00176, "gade__dev__cpu__instructionsS");
   u00177 : constant Version_32 := 16#f64b89a4#;
   pragma Export (C, u00177, "ada__integer_text_ioB");
   u00178 : constant Version_32 := 16#b85ee1d1#;
   pragma Export (C, u00178, "ada__integer_text_ioS");
   u00179 : constant Version_32 := 16#f6fdca1c#;
   pragma Export (C, u00179, "ada__text_io__integer_auxB");
   u00180 : constant Version_32 := 16#b9793d30#;
   pragma Export (C, u00180, "ada__text_io__integer_auxS");
   u00181 : constant Version_32 := 16#181dc502#;
   pragma Export (C, u00181, "ada__text_io__generic_auxB");
   u00182 : constant Version_32 := 16#a6c327d3#;
   pragma Export (C, u00182, "ada__text_io__generic_auxS");
   u00183 : constant Version_32 := 16#b10ba0c7#;
   pragma Export (C, u00183, "system__img_biuB");
   u00184 : constant Version_32 := 16#faff9b35#;
   pragma Export (C, u00184, "system__img_biuS");
   u00185 : constant Version_32 := 16#4e06ab0c#;
   pragma Export (C, u00185, "system__img_llbB");
   u00186 : constant Version_32 := 16#bb388bcb#;
   pragma Export (C, u00186, "system__img_llbS");
   u00187 : constant Version_32 := 16#9dca6636#;
   pragma Export (C, u00187, "system__img_lliB");
   u00188 : constant Version_32 := 16#19143a2a#;
   pragma Export (C, u00188, "system__img_lliS");
   u00189 : constant Version_32 := 16#a756d097#;
   pragma Export (C, u00189, "system__img_llwB");
   u00190 : constant Version_32 := 16#1254a85d#;
   pragma Export (C, u00190, "system__img_llwS");
   u00191 : constant Version_32 := 16#eb55dfbb#;
   pragma Export (C, u00191, "system__img_wiuB");
   u00192 : constant Version_32 := 16#94be1ca7#;
   pragma Export (C, u00192, "system__img_wiuS");
   u00193 : constant Version_32 := 16#d763507a#;
   pragma Export (C, u00193, "system__val_intB");
   u00194 : constant Version_32 := 16#40fe45c4#;
   pragma Export (C, u00194, "system__val_intS");
   u00195 : constant Version_32 := 16#1d9142a4#;
   pragma Export (C, u00195, "system__val_unsB");
   u00196 : constant Version_32 := 16#2c75fe43#;
   pragma Export (C, u00196, "system__val_unsS");
   u00197 : constant Version_32 := 16#1a74a354#;
   pragma Export (C, u00197, "system__val_lliB");
   u00198 : constant Version_32 := 16#927f895b#;
   pragma Export (C, u00198, "system__val_lliS");
   u00199 : constant Version_32 := 16#dbf4721a#;
   pragma Export (C, u00199, "gade__dev__cpu__arithmeticB");
   u00200 : constant Version_32 := 16#c0726c58#;
   pragma Export (C, u00200, "gade__dev__cpu__arithmeticS");
   u00201 : constant Version_32 := 16#8a59f678#;
   pragma Export (C, u00201, "gade__dev__cpu__bitwiseB");
   u00202 : constant Version_32 := 16#69ebe93c#;
   pragma Export (C, u00202, "gade__dev__cpu__bitwiseS");
   u00203 : constant Version_32 := 16#f5cb55cb#;
   pragma Export (C, u00203, "gade__dev__cpu__logicB");
   u00204 : constant Version_32 := 16#7c339c2a#;
   pragma Export (C, u00204, "gade__dev__cpu__logicS");
   u00205 : constant Version_32 := 16#1d87b4a7#;
   pragma Export (C, u00205, "gade__dev__cpu__instructions__execB");
   u00206 : constant Version_32 := 16#693235c4#;
   pragma Export (C, u00206, "gade__dev__cpu__instructions__execS");
   u00207 : constant Version_32 := 16#eabeb7c7#;
   pragma Export (C, u00207, "gade__dev__cpu__instructions__tableS");
   u00208 : constant Version_32 := 16#99aaaf37#;
   pragma Export (C, u00208, "test_directoriesS");
   u00209 : constant Version_32 := 16#01a73f89#;
   pragma Export (C, u00209, "ada__command_lineB");
   u00210 : constant Version_32 := 16#3cdef8c9#;
   pragma Export (C, u00210, "ada__command_lineS");
   u00211 : constant Version_32 := 16#f1b3ad16#;
   pragma Export (C, u00211, "ada__directoriesB");
   u00212 : constant Version_32 := 16#71554425#;
   pragma Export (C, u00212, "ada__directoriesS");
   u00213 : constant Version_32 := 16#0d7f1a43#;
   pragma Export (C, u00213, "ada__calendarB");
   u00214 : constant Version_32 := 16#5b279c75#;
   pragma Export (C, u00214, "ada__calendarS");
   u00215 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00215, "interfaces__cB");
   u00216 : constant Version_32 := 16#70be4e8c#;
   pragma Export (C, u00216, "interfaces__cS");
   u00217 : constant Version_32 := 16#a6535153#;
   pragma Export (C, u00217, "system__os_primitivesB");
   u00218 : constant Version_32 := 16#82d47e8d#;
   pragma Export (C, u00218, "system__os_primitivesS");
   u00219 : constant Version_32 := 16#8f218b8f#;
   pragma Export (C, u00219, "ada__calendar__formattingB");
   u00220 : constant Version_32 := 16#67ade573#;
   pragma Export (C, u00220, "ada__calendar__formattingS");
   u00221 : constant Version_32 := 16#e3cca715#;
   pragma Export (C, u00221, "ada__calendar__time_zonesB");
   u00222 : constant Version_32 := 16#6dc27f8f#;
   pragma Export (C, u00222, "ada__calendar__time_zonesS");
   u00223 : constant Version_32 := 16#faa9a7b2#;
   pragma Export (C, u00223, "system__val_realB");
   u00224 : constant Version_32 := 16#f67218ea#;
   pragma Export (C, u00224, "system__val_realS");
   u00225 : constant Version_32 := 16#b2a569d2#;
   pragma Export (C, u00225, "system__exn_llfB");
   u00226 : constant Version_32 := 16#b425d427#;
   pragma Export (C, u00226, "system__exn_llfS");
   u00227 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00227, "system__float_controlB");
   u00228 : constant Version_32 := 16#e8a72cc7#;
   pragma Export (C, u00228, "system__float_controlS");
   u00229 : constant Version_32 := 16#582b098c#;
   pragma Export (C, u00229, "system__powten_tableS");
   u00230 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00230, "ada__charactersS");
   u00231 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00231, "ada__characters__handlingB");
   u00232 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00232, "ada__characters__handlingS");
   u00233 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00233, "ada__characters__latin_1S");
   u00234 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00234, "ada__stringsS");
   u00235 : constant Version_32 := 16#e2ea8656#;
   pragma Export (C, u00235, "ada__strings__mapsB");
   u00236 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00236, "ada__strings__mapsS");
   u00237 : constant Version_32 := 16#a7325af6#;
   pragma Export (C, u00237, "system__bit_opsB");
   u00238 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00238, "system__bit_opsS");
   u00239 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00239, "ada__strings__maps__constantsS");
   u00240 : constant Version_32 := 16#ab4ad33a#;
   pragma Export (C, u00240, "ada__directories__validityB");
   u00241 : constant Version_32 := 16#d34bdf62#;
   pragma Export (C, u00241, "ada__directories__validityS");
   u00242 : constant Version_32 := 16#e5480ede#;
   pragma Export (C, u00242, "ada__strings__fixedB");
   u00243 : constant Version_32 := 16#a86b22b3#;
   pragma Export (C, u00243, "ada__strings__fixedS");
   u00244 : constant Version_32 := 16#2eb48a6d#;
   pragma Export (C, u00244, "ada__strings__searchB");
   u00245 : constant Version_32 := 16#c1ab8667#;
   pragma Export (C, u00245, "ada__strings__searchS");
   u00246 : constant Version_32 := 16#3791e504#;
   pragma Export (C, u00246, "ada__strings__unboundedB");
   u00247 : constant Version_32 := 16#9fdb1809#;
   pragma Export (C, u00247, "ada__strings__unboundedS");
   u00248 : constant Version_32 := 16#933d1555#;
   pragma Export (C, u00248, "system__compare_array_unsigned_8B");
   u00249 : constant Version_32 := 16#a1581e76#;
   pragma Export (C, u00249, "system__compare_array_unsigned_8S");
   u00250 : constant Version_32 := 16#97d13ec4#;
   pragma Export (C, u00250, "system__address_operationsB");
   u00251 : constant Version_32 := 16#1b57d1c8#;
   pragma Export (C, u00251, "system__address_operationsS");
   u00252 : constant Version_32 := 16#020a3f4d#;
   pragma Export (C, u00252, "system__atomic_countersB");
   u00253 : constant Version_32 := 16#bc074276#;
   pragma Export (C, u00253, "system__atomic_countersS");
   u00254 : constant Version_32 := 16#9bfd1d50#;
   pragma Export (C, u00254, "system__file_attributesS");
   u00255 : constant Version_32 := 16#e073a45b#;
   pragma Export (C, u00255, "system__os_constantsS");
   u00256 : constant Version_32 := 16#908d8e33#;
   pragma Export (C, u00256, "system__regexpB");
   u00257 : constant Version_32 := 16#2b69c837#;
   pragma Export (C, u00257, "system__regexpS");
   u00258 : constant Version_32 := 16#d6d40b18#;
   pragma Export (C, u00258, "filesystemB");
   u00259 : constant Version_32 := 16#06594636#;
   pragma Export (C, u00259, "filesystemS");
   u00260 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00260, "ada__containersS");
   u00261 : constant Version_32 := 16#bcec81df#;
   pragma Export (C, u00261, "ada__containers__helpersB");
   u00262 : constant Version_32 := 16#4adfc5eb#;
   pragma Export (C, u00262, "ada__containers__helpersS");
   u00263 : constant Version_32 := 16#32fd0300#;
   pragma Export (C, u00263, "system__strings__stream_opsB");
   u00264 : constant Version_32 := 16#55d4bd57#;
   pragma Export (C, u00264, "system__strings__stream_opsS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  gnat%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_counters%s
   --  system.atomic_counters%b
   --  system.byte_swapping%s
   --  gnat.byte_swapping%s
   --  gnat.byte_swapping%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_biu%s
   --  system.img_biu%b
   --  system.img_llb%s
   --  system.img_llb%b
   --  system.img_llw%s
   --  system.img_llw%b
   --  system.img_wiu%s
   --  system.img_wiu%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.wch_stw%s
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  ada.exceptions%s
   --  system.wch_stw%b
   --  ada.exceptions.traceback%s
   --  system.soft_links%s
   --  system.exception_table%s
   --  system.exception_table%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.secondary_stack%s
   --  system.address_image%s
   --  system.soft_links%b
   --  ada.exceptions.last_chance_handler%s
   --  system.memory%s
   --  system.memory%b
   --  ada.exceptions.traceback%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.secondary_stack%b
   --  system.address_image%b
   --  ada.exceptions.last_chance_handler%b
   --  system.standard_library%b
   --  ada.exceptions%b
   --  ada.command_line%s
   --  ada.command_line%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.os_constants%s
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_llu%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.communication%s
   --  system.communication%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.containers.helpers%s
   --  ada.containers.helpers%b
   --  system.file_io%s
   --  system.file_io%b
   --  ada.streams.stream_io%s
   --  ada.streams.stream_io%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.finalization_masters%b
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.storage_pools.subpools%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.val_lli%s
   --  system.val_lli%b
   --  system.val_real%s
   --  system.val_real%b
   --  system.val_uns%s
   --  system.val_uns%b
   --  system.val_int%s
   --  system.val_int%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.time_zones%s
   --  ada.calendar.time_zones%b
   --  ada.calendar.formatting%s
   --  ada.calendar.formatting%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  ada.text_io.integer_aux%s
   --  ada.text_io.integer_aux%b
   --  ada.integer_text_io%s
   --  ada.integer_text_io%b
   --  gnat.secure_hashes%s
   --  gnat.secure_hashes%b
   --  gnat.secure_hashes.md5%s
   --  gnat.secure_hashes.md5%b
   --  gnat.md5%s
   --  gnat.md5%b
   --  system.assertions%s
   --  system.assertions%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  ada.characters.handling%s
   --  ada.characters.handling%b
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.strings.fixed%s
   --  ada.strings.fixed%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  system.file_attributes%s
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.regexp%s
   --  system.regexp%b
   --  ada.directories%s
   --  ada.directories.validity%s
   --  ada.directories.validity%b
   --  ada.directories%b
   --  system.sequential_io%s
   --  system.sequential_io%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  bitmap%s
   --  bitmap.buffer%s
   --  bitmap.color_conversion%s
   --  bitmap.color_conversion%b
   --  bitmap.soft_drawing%s
   --  bitmap.soft_drawing%b
   --  bitmap.memory_mapped%s
   --  bitmap.memory_mapped%b
   --  bitmap.file_io%s
   --  bitmap.file_io%b
   --  compare_files%s
   --  compare_files%b
   --  filesystem%s
   --  filesystem%b
   --  gade%s
   --  gade.dev%s
   --  gade.dev.cpu%s
   --  gade.dev.cpu%b
   --  gade.dev.cpu.arithmetic%s
   --  gade.dev.cpu.arithmetic%b
   --  gade.dev.cpu.bitwise%s
   --  gade.dev.cpu.bitwise%b
   --  gade.dev.cpu.logic%s
   --  gade.dev.cpu.logic%b
   --  gade.dev.external_ram%s
   --  gade.dev.external_ram%b
   --  gade.dev.video%s
   --  gade.input_reader%s
   --  gade.dev.joypad%s
   --  gade.dev.joypad%b
   --  gade.video_buffer%s
   --  gade.dev.oam%s
   --  gade.dev.oam%b
   --  gade.dev.video.tile_buffer%s
   --  gade.dev.video.tile_map%s
   --  gade.dev.video.tile_map%b
   --  gade.dev.vram%s
   --  gade.dev.vram%b
   --  gade.dev.video.tile_buffer%b
   --  gade.dev.video.background_buffer%s
   --  gade.dev.video.background_buffer%b
   --  gade.dev.video.sprites%s
   --  gade.dev.video.sprites%b
   --  gade.dev.video.window%s
   --  gade.dev.video.window%b
   --  gade.dev.cartridge%s
   --  gade.dev.timer%s
   --  gade.dev.interrupts%s
   --  gade.dev.display%s
   --  gade.gb%s
   --  gade.dev.timer%b
   --  gade.gb.memory_map%s
   --  gade.gb.memory_map%b
   --  gade.dev.interrupts%b
   --  gade.dev.display%b
   --  gade.dev.cartridge%b
   --  gade.gb%b
   --  gade.dev.cpu.instructions%s
   --  gade.dev.cpu.instructions%b
   --  gade.dev.cpu.instructions.table%s
   --  gade.dev.cpu.instructions.exec%s
   --  gade.dev.cpu.instructions.exec%b
   --  gade.interfaces%s
   --  gade.interfaces%b
   --  test_directories%s
   --  test_lcd_output%s
   --  test_lcd_output%b
   --  tc_instr_timing%b
   --  END ELABORATION ORDER

end ada_main;
