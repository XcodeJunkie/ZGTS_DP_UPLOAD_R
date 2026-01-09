*&---------------------------------------------------------------------*
*& Include          ZGTS_DP_UPLOAD_CD1
*&---------------------------------------------------------------------*
INTERFACE gif_const.
  CONSTANTS:
    BEGIN OF gc_path,
      main    TYPE /sapsll/path_local_appserver VALUE 'Z_Reguvis_Files',
      archive TYPE /sapsll/path_local_appserver VALUE 'Archive',
    END OF gc_path,
    BEGIN OF gc_kind,
      params TYPE rsscr_kind VALUE 'P',
      selopt TYPE rsscr_kind VALUE 'S',
    END OF gc_kind,
    pcre_delimiter TYPE char3 VALUE '_I_',
    BEGIN OF gc_content,
      alnde TYPE zgts_dp_content VALUE 'ALNDE',
      splde TYPE zgts_dp_content VALUE 'SPLDE',
      splus TYPE zgts_dp_content VALUE 'SPLUS',
      meaeu TYPE zgts_dp_content VALUE 'MEAEU',
      meade TYPE zgts_dp_content VALUE 'MEADE',
      duaeu TYPE zgts_dp_content VALUE 'DUAEU',
      tareu TYPE zgts_dp_content VALUE 'TAREU',
      taruk TYPE zgts_dp_content VALUE 'TARUK',
      tarus TYPE zgts_dp_content VALUE 'TARUS',
      cclus TYPE zgts_dp_content VALUE 'CCLUS',
      reteu TYPE zgts_dp_content VALUE 'RETEU',
      retuk TYPE zgts_dp_content VALUE 'RETUK',
    END OF gc_content,
    BEGIN OF gc_procs,
      all    TYPE zgts_dp_process_opt VALUE 'A',
      newest TYPE zgts_dp_process_opt VALUE 'N',
    END OF gc_procs,
    BEGIN OF gc_program,
      upload_002 TYPE program VALUE '/SAPSLL/NSC_NUM_UPLOAD_002',
      upload_101 TYPE program VALUE '/SAPSLL/NSC_NUM_UPLOAD_101',
      upload_102 TYPE program VALUE '/SAPSLL/NSC_NUM_UPLOAD_102',
      retarif    TYPE program VALUE '/SAPSLL/PR_RETARIF_XML_CUS',
      spl        TYPE program VALUE '/SAPSLL/SPL_UPLOAD_IXML',
      measures   TYPE program VALUE '/SAPSLL/SLLNS_XML_MEASURES_UPL',
    END OF gc_program.
ENDINTERFACE.


INTERFACE gif_message_sender.
  EVENTS: message_occured EXPORTING VALUE(i_probclass) TYPE balprobcl DEFAULT '2'.
ENDINTERFACE.


INTERFACE gif_filehandler.
  INTERFACES: gif_message_sender.

  CLASS-DATA:
    mv_path_sep TYPE char1 READ-ONLY.

  DATA:
    ms_dp_upload TYPE zgts_dp_upload.

  METHODS:
    is_local
      RETURNING VALUE(rv_is_local) TYPE abap_bool,

    start_upload
      IMPORTING iv_path TYPE csequence,

    move_file_to_archive
      IMPORTING iv_file TYPE csequence
                iv_full TYPE csequence.

  EVENTS:
    file_found
      EXPORTING VALUE(ev_file) TYPE csequence OPTIONAL
                VALUE(ev_path) TYPE csequence OPTIONAL
                VALUE(ev_full) TYPE csequence OPTIONAL,

    upload_finished.
ENDINTERFACE.


CLASS gcl_file_local DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES: gif_filehandler.

    CLASS-METHODS:
      class_constructor.

    METHODS:
      constructor
        IMPORTING is_dp_upload TYPE zgts_dp_upload.

  PRIVATE SECTION.
    DATA: mv_base_dir TYPE string,
          mv_path     TYPE string.

ENDCLASS.


CLASS gcl_file_server DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES: gif_filehandler.

    CLASS-METHODS:
      class_constructor.

    METHODS:
      constructor
        IMPORTING is_dp_upload TYPE zgts_dp_upload.

  PRIVATE SECTION.
    TYPES:
      tt_lines TYPE STANDARD TABLE OF bdi_line WITH EMPTY KEY,

      BEGIN OF ts_file,
        name     TYPE eps2filnam,
        mtime    TYPE p LENGTH 6 DECIMALS 0,
        mod_date TYPE d,
        mod_time TYPE t,
        full     TYPE /sapsll/path_local_appserver,
        path     TYPE /sapsll/path_local_appserver,
        content  TYPE c LENGTH 20,
        type     TYPE c LENGTH 10,
        errno    TYPE c LENGTH 3,
        errmsg   TYPE c LENGTH 40,
      END OF ts_file,
      tt_files TYPE STANDARD TABLE OF ts_file WITH DEFAULT KEY,

      BEGIN OF ts_path,
        path TYPE eps2filnam,
      END OF ts_path,
      tt_paths TYPE STANDARD TABLE OF ts_path WITH DEFAULT KEY.


    DATA: mt_path     TYPE STANDARD TABLE OF string,
          mv_base_dir TYPE string.

    METHODS:
      scan_directory
        CHANGING ct_paths TYPE tt_paths
                 ct_files TYPE tt_files,

      read_text_file
        IMPORTING iv_file_path    TYPE /sapsll/path_local_appserver
        EXPORTING et_file_content TYPE tt_lines
                  ev_file_length  TYPE i,

      write_text_file
        IMPORTING iv_file_path    TYPE /sapsll/path_local_appserver
                  it_file_content TYPE tt_lines
        EXPORTING ev_file_length  TYPE i,

      raise_when_matches
        IMPORTING is_file TYPE ts_file.

ENDCLASS.


CLASS gcl_email_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING it_email TYPE /sapsll/ad_smtpadr_r_t,

      on_file_found FOR EVENT file_found OF gif_filehandler IMPORTING ev_file,

      on_upload_finished FOR EVENT upload_finished OF gif_filehandler.

  PRIVATE SECTION.
    DATA: mr_mail_request TYPE REF TO cl_bcs,
          mt_mail_body    TYPE soli_tab.

ENDCLASS.


CLASS gcl_content_abstr DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES: gif_message_sender.

    METHODS:
      on_file_found ABSTRACT FOR EVENT file_found OF gif_filehandler
        IMPORTING
          sender
          ev_file
          ev_full
          ev_path.

  PROTECTED SECTION.
    METHODS:
      process_file
        IMPORTING
          ir_sender  TYPE REF TO gif_filehandler
          iv_file    TYPE csequence
          iv_full    TYPE csequence
          iv_program TYPE program
          it_params  TYPE btc_t_templ_var.

ENDCLASS.


CLASS gcl_num_upload_002 DEFINITION FINAL INHERITING FROM gcl_content_abstr.
  PUBLIC SECTION.
    METHODS:
      on_file_found REDEFINITION.
ENDCLASS.


CLASS gcl_num_upload_101 DEFINITION FINAL INHERITING FROM gcl_content_abstr.
  PUBLIC SECTION.
    METHODS:
      on_file_found REDEFINITION.
ENDCLASS.


CLASS gcl_num_upload_102 DEFINITION FINAL INHERITING FROM gcl_content_abstr.
  PUBLIC SECTION.
    METHODS:
      on_file_found REDEFINITION.
ENDCLASS.


CLASS gcl_retarif_xml_cus DEFINITION FINAL INHERITING FROM gcl_content_abstr.
  PUBLIC SECTION.
    METHODS:
      on_file_found REDEFINITION.
ENDCLASS.


CLASS gcl_spl_upload_ixml DEFINITION FINAL INHERITING FROM gcl_content_abstr.
  PUBLIC SECTION.
    METHODS:
      on_file_found REDEFINITION.
ENDCLASS.


CLASS gcl_xml_measures_upl DEFINITION FINAL INHERITING FROM gcl_content_abstr.
  PUBLIC SECTION.
    METHODS:
      on_file_found REDEFINITION.
ENDCLASS.




*
*CLASS gcl_type_abstr DEFINITION ABSTRACT.
*  PUBLIC SECTION.
*    INTERFACES: gif_message_sender.
*
*    METHODS:
*      constructor
*        IMPORTING iv_data_provider TYPE /sapsll/bpdpv
*                  ir_mode          TYPE REF TO gif_mode
*                  iv_program       TYPE program,
*
*      on_file_found FOR EVENT file_found OF gif_filehandler
*        IMPORTING
*          sender
*          ev_file
*          ev_full
*          ev_path.
*
*  PROTECTED SECTION.
*    DATA: mv_data_provider TYPE /sapsll/bpdpv,
*          mr_mode          TYPE REF TO gif_mode,
*          mv_program       TYPE program.
*
*ENDCLASS.
*
*
*CLASS gcl_tariff DEFINITION INHERITING FROM gcl_type_abstr FINAL.
*  PUBLIC SECTION.
*    METHODS:
*      constructor
*        IMPORTING ir_mode          TYPE REF TO gif_mode
*                  iv_data_provider TYPE /sapsll/bpdpv
*                  iv_stccs         TYPE /sapsll/stccs
*                  it_spras         TYPE /sapsll/spras_r_t,
*
*      on_file_found REDEFINITION.
*
*  PRIVATE SECTION.
*    DATA:
*      mv_stccs TYPE /sapsll/stccs,
*      mv_prog  TYPE program,
*      mt_spras TYPE /sapsll/spras_r_t.
*
*ENDCLASS.
*
*
*CLASS gcl_control_class DEFINITION INHERITING FROM gcl_type_abstr FINAL.
*  PUBLIC SECTION.
*    METHODS:
*      constructor
*        IMPORTING ir_mode          TYPE REF TO gif_mode
*                  iv_data_provider TYPE /sapsll/bpdpv
*                  iv_stccs         TYPE /sapsll/stccs
*                  it_spras         TYPE /sapsll/spras_r_t.
*ENDCLASS.
*
*
*CLASS gcl_spl DEFINITION FINAL INHERITING FROM gcl_type_abstr.
*  PUBLIC SECTION.
*    METHODS:
*      constructor
*        IMPORTING ir_mode          TYPE REF TO gif_mode
*                  iv_data_provider TYPE /sapsll/bpdpv
*                  iv_lgreg         TYPE /sapsll/lgreg_n
*                  it_ltype         TYPE /sapsll/spcat_r_t
*                  it_spgrp         TYPE /sapsll/spgrp_r_t,
*
*      on_file_found REDEFINITION.
*
*  PRIVATE SECTION.
*    DATA:
*      mt_ltype TYPE /sapsll/spcat_r_t,
*      mt_spgrp TYPE /sapsll/spgrp_r_t,
*      mv_lgreg TYPE /sapsll/lgreg_n.
*
*ENDCLASS.
*
*
*CLASS gcl_measures DEFINITION FINAL INHERITING FROM gcl_type_abstr.
*  PUBLIC SECTION.
*    METHODS:
*      constructor
*        IMPORTING ir_mode          TYPE REF TO gif_mode
*                  iv_data_provider TYPE /sapsll/bpdpv
*                  iv_stcsm         TYPE /sapsll/stcsm,
*
*      on_file_found REDEFINITION.
*
*  PRIVATE SECTION.
*    DATA:
*      mv_stcsm TYPE /sapsll/stcsm.
*
*ENDCLASS.
