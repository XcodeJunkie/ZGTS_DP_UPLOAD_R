*&---------------------------------------------------------------------*
*& Include          ZGTS_DP_UPLOAD_CI1
*&---------------------------------------------------------------------*

CLASS gcl_file_local IMPLEMENTATION.
  METHOD class_constructor.
    cl_gui_frontend_services=>get_file_separator( CHANGING file_separator = gif_filehandler~mv_path_sep ).
  ENDMETHOD.

  METHOD constructor.
    gif_filehandler~ms_dp_upload = is_dp_upload.
  ENDMETHOD.

  METHOD gif_filehandler~is_local.
    rv_is_local = abap_true.
  ENDMETHOD.

  METHOD gif_filehandler~move_file_to_archive.
  ENDMETHOD.

  METHOD gif_filehandler~start_upload.
    " Local declarations
    DATA: lt_files TYPE STANDARD TABLE OF file_info WITH DEFAULT KEY,
          lv_count TYPE i.

    " Local constants
    CONSTANTS: lc_xml_only TYPE string VALUE '*.xml'.

    " Get all *.xml files from the chosen directory
    cl_gui_frontend_services=>directory_list_files(
      EXPORTING
        directory                   = iv_path
        filter                      = lc_xml_only
        files_only                  = abap_false
      CHANGING
        file_table                  = lt_files
        count                       = lv_count
      EXCEPTIONS
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4 ).

    IF sy-subrc <> 0 OR lv_count = 0.
      RETURN.
    ENDIF.

    MESSAGE i024(cbc_common) WITH mv_path INTO DATA(lv_dummy).
    RAISE EVENT gif_message_sender~message_occured.

    " Process all files and trigger upload for each
    LOOP AT lt_files INTO DATA(ls_file).
      RAISE EVENT gif_filehandler~file_found
        EXPORTING
          ev_file = ls_file-filename
          ev_path = mv_path
          ev_full = |{ iv_path }{ gif_filehandler~mv_path_sep }{ ls_file-filename }|.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


CLASS gcl_file_server IMPLEMENTATION.
  METHOD class_constructor.
    gif_filehandler~mv_path_sep = cl_ilm_stor_util=>dir_separator( ).
  ENDMETHOD.

  METHOD constructor.
    gif_filehandler~ms_dp_upload = is_dp_upload.
  ENDMETHOD.

  METHOD gif_filehandler~is_local.
    rv_is_local = abap_false.
  ENDMETHOD.

  METHOD gif_filehandler~start_upload.

    DATA: lt_files TYPE tt_files.

    DATA(lt_paths) = VALUE tt_paths( ( path = iv_path ) ).
    DO.
      IF lines( lt_paths ) = 0.
        EXIT.
      ENDIF.

      scan_directory(
        CHANGING ct_paths = lt_paths
                 ct_files = lt_files ).
    ENDDO.

    SORT lt_files BY path ASCENDING.

    LOOP AT lt_files INTO DATA(ls_file)
                      GROUP BY ( content = ls_file-content )
           REFERENCE INTO DATA(lr_file_grp).
      CASE gif_filehandler~ms_dp_upload-procs.
        WHEN gif_const=>gc_procs-newest.
          ls_file = REDUCE ts_file(
              INIT s_file = VALUE ts_file( )
              FOR <s_file> IN GROUP lr_file_grp
              NEXT s_file = COND #( LET  v_ver_curr = substring_from( val  = <s_file>-name pcre = gif_const=>pcre_delimiter )
                                         v_ver_high = substring_from( val  = s_file-name   pcre = gif_const=>pcre_delimiter ) IN
                                    WHEN v_ver_curr > v_ver_high THEN <s_file>
                                    ELSE s_file ) ).
          raise_when_matches( ls_file ).

        WHEN OTHERS.
          " Take all files
          LOOP AT GROUP lr_file_grp INTO ls_file.
            raise_when_matches( ls_file ).
          ENDLOOP.
      ENDCASE.
    ENDLOOP.

    RAISE EVENT gif_filehandler~upload_finished.

  ENDMETHOD.

  METHOD raise_when_matches.
    IF matches( val  = is_file-name
                pcre = gif_filehandler~ms_dp_upload-regex ).
      RAISE EVENT gif_filehandler~file_found
        EXPORTING ev_file = is_file-name
                  ev_full = is_file-full
                  ev_path = is_file-path.
    ENDIF.
  ENDMETHOD.

  METHOD read_text_file.

    DATA: ls_line        TYPE LINE OF tt_lines,
          lv_line_length TYPE i,
          lv_file_length TYPE i.

    CLEAR: et_file_content,
           ev_file_length.

    OPEN DATASET iv_file_path FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DO.
      CLEAR: ls_line,
             lv_line_length.

      READ DATASET iv_file_path INTO ls_line LENGTH lv_line_length.
      IF sy-subrc = 0.
        APPEND ls_line TO et_file_content.
        ADD lv_line_length TO ev_file_length.

      ELSEIF sy-subrc = 8.
        EXIT.

      ELSE.
        APPEND ls_line TO et_file_content.
        ADD lv_line_length TO ev_file_length.
        EXIT.

      ENDIF.
    ENDDO.

    CLOSE DATASET iv_file_path.

  ENDMETHOD.

  METHOD gif_filehandler~move_file_to_archive.

    TYPES: tt_path TYPE STANDARD TABLE OF /sapsll/path_local_appserver WITH EMPTY KEY.

    DATA: lt_file_content  TYPE tt_lines,
          lv_target        TYPE /sapsll/path_local_appserver,
          lv_source_length TYPE i,
          lv_target_length TYPE i.

    " 1. Copy file
    read_text_file(
      EXPORTING iv_file_path    = iv_full
      IMPORTING et_file_content = lt_file_content
                ev_file_length  = lv_source_length ).


    DATA(lt_path) = VALUE tt_path( ( gif_const=>gc_path-main )
                                   ( gif_const=>gc_path-archive )
                                   ( iv_file ) ).

    lv_target = gif_filehandler~mv_path_sep && concat_lines_of( table = lt_path
                                                                sep   = gif_filehandler~mv_path_sep ).

    write_text_file(
      EXPORTING iv_file_path    = lv_target
                it_file_content = lt_file_content
      IMPORTING ev_file_length  = lv_target_length ).

    IF lv_source_length = lv_target_length.
      " 2. Delete source file
      DELETE DATASET iv_full.

    ELSE.
      DELETE DATASET lv_target.

    ENDIF.

  ENDMETHOD.

  METHOD write_text_file.
    CLEAR: ev_file_length.

    OPEN DATASET iv_file_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_file_content INTO DATA(ls_line).
      TRANSFER ls_line TO iv_file_path.
    ENDLOOP.
    CLOSE DATASET iv_file_path.

    read_text_file(
      EXPORTING iv_file_path    = iv_file_path
      IMPORTING ev_file_length  = ev_file_length ).

  ENDMETHOD.

  METHOD scan_directory.

    DATA: ls_file      TYPE ts_file,
          lv_file_mask TYPE epsfilnam,
          lv_timec     TYPE c LENGTH 8.              " hh:mm:ss.

    DATA(lv_path) = ct_paths[ 1 ]-path.
    DELETE ct_paths INDEX 1.

    CALL 'C_DIR_READ_FINISH'                  " just to be sure
      ID 'ERRNO'  FIELD ls_file-errno
      ID 'ERRMSG' FIELD ls_file-errmsg.

    CALL 'C_DIR_READ_START'
      ID 'DIR'    FIELD lv_path
      ID 'FILE'   FIELD lv_file_mask
      ID 'ERRNO'  FIELD ls_file-errno
      ID 'ERRMSG' FIELD ls_file-errmsg.

    IF sy-subrc <> 0.
      " Close directory
      CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD ls_file-errno
        ID 'ERRMSG' FIELD ls_file-errmsg.
      RETURN.
    ENDIF.

    DO.
      CALL 'C_DIR_READ_NEXT'
        ID 'TYPE'   FIELD ls_file-type
        ID 'NAME'   FIELD ls_file-name
        ID 'MTIME'  FIELD ls_file-mtime
        ID 'ERRNO'  FIELD ls_file-errno
        ID 'ERRMSG' FIELD ls_file-errmsg.

      IF sy-subrc = 0.
        " Regular file
        IF ( ls_file-type(1) = 'f' OR
             ls_file-type(1) = 'F' ) AND matches( val  = ls_file-name
                                                  pcre = '[\w-]+\.xml' ).
          CLEAR lv_timec.
          PERFORM p6_to_date_time_tz IN PROGRAM rstr0400 USING ls_file-mtime
                                                               lv_timec
                                                               ls_file-mod_date.
          REPLACE ALL OCCURRENCES OF ':'
                      IN lv_timec
                      WITH ''
                      IN CHARACTER MODE.
          ls_file-mod_time = lv_timec.
          ls_file-content = substring_to( val  = ls_file-name
                                          pcre = '_I_' ).

          APPEND VALUE #( BASE ls_file
            name = ls_file-name
            full = lv_path && gif_filehandler~mv_path_sep && ls_file-name
            path = lv_path
          ) TO ct_files.

          " Directory
        ELSEIF ( ls_file-type(1) = 'd' OR
                 ls_file-type(1) = 'D' ) AND NOT matches( val  = ls_file-name
                                                          pcre = '^\.+').
          APPEND lv_path && gif_filehandler~mv_path_sep && ls_file-name TO ct_paths.

        ENDIF.

      ELSEIF sy-subrc = 1.
        EXIT.

      ELSE.
        CONTINUE.

      ENDIF.
    ENDDO.

    " Close directory
    CALL 'C_DIR_READ_FINISH'
      ID 'ERRNO'  FIELD ls_file-errno
      ID 'ERRMSG' FIELD ls_file-errmsg.

  ENDMETHOD.
ENDCLASS.


CLASS gcl_email_handler IMPLEMENTATION.
  METHOD constructor.
    TRY.
        mr_mail_request = cl_bcs=>create_persistent( ).

        " This could be done better with SO10 texts, but no time...
        APPEND TEXT-001 TO mt_mail_body.
        APPEND TEXT-002 && px_simu TO mt_mail_body.
        APPEND TEXT-003 TO mt_mail_body.

      CATCH cx_send_req_bcs.
        CLEAR mr_mail_request.

    ENDTRY.
  ENDMETHOD.

  METHOD on_file_found.
    APPEND ev_file TO mt_mail_body.
  ENDMETHOD.

  METHOD on_upload_finished.
    " Mail shall only be sent, when email recipients are maintained
    IF s_email[] IS INITIAL.
      RETURN.
    ENDIF.

    " Mail subject
    DATA(lv_mail_subject) = CONV so_obj_des( TEXT-001 ).

    " Upload beendet.
    APPEND TEXT-999 TO mt_mail_body.

    TRY.
        " Mail document
        DATA(lr_document) = cl_document_bcs=>create_document(
          i_type    = 'RAW'
          i_text    = mt_mail_body
          i_subject = lv_mail_subject ).

        mr_mail_request->set_document( lr_document ).

        " Mail recipient
        LOOP AT s_email INTO DATA(ls_email).
          DATA(lr_recipient) = cl_cam_address_bcs=>create_internet_address( ls_email-low ).
          mr_mail_request->add_recipient( lr_recipient ).
        ENDLOOP.

        " Keine Lesebestätigung
        mr_mail_request->set_status_attributes( i_requested_status = 'N' ).

        mr_mail_request->set_send_immediately( abap_true ).

        DATA(lv_sent_to_all) = mr_mail_request->send( i_with_error_screen = abap_false ).
        IF lv_sent_to_all = abap_true.
          " Mail sent successfully
        ENDIF.

        COMMIT WORK.

      CATCH cx_document_bcs
            cx_address_bcs
            cx_send_req_bcs.

    ENDTRY.
  ENDMETHOD.
ENDCLASS.


CLASS gcl_content_abstr IMPLEMENTATION.
  METHOD process_file.
    WRITE:/ |Upload von Datei { iv_file }|.

    IF px_simu = abap_false.
      SUBMIT (iv_program) WITH SELECTION-TABLE it_params AND RETURN.

      ir_sender->move_file_to_archive(
        iv_file = iv_file
        iv_full = iv_full ).

    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS gcl_num_upload_002 IMPLEMENTATION.
  METHOD on_file_found.
    DATA(lv_local_file) = sender->is_local( ).

    DATA(lt_params) = VALUE btc_t_templ_var(
      ( selname = 'P_BP'     kind = gif_const=>gc_kind-params low = p_dp )
      ( selname = 'P_STCCS'  kind = gif_const=>gc_kind-params low = p_stcc2 )
      ( selname = 'P_PATH'   kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_true THEN ev_full ELSE space ) )
      ( selname = 'P_PATH2'  kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_false THEN ev_full ELSE space ) )
      ( selname = 'P_PCKGC'  kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_NSCUP'  kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_SIMUL'  kind = gif_const=>gc_kind-params low = abap_false )
    ).
    lt_params = VALUE #( BASE lt_params FOR ls_langu IN s_langu[] ( selname = 'S_LANGU' kind = gif_const=>gc_kind-selopt
      sign   = ls_langu-sign
      option = ls_langu-option
      low    = ls_langu-low
      high   = ls_langu-high )
    ).

    process_file(
      ir_sender  = sender
      iv_file    = ev_file
      iv_full    = ev_full
      iv_program = gif_const=>gc_program-upload_002
      it_params  = lt_params
    ).

  ENDMETHOD.
ENDCLASS.


CLASS gcl_num_upload_101 IMPLEMENTATION.
  METHOD on_file_found.
    DATA(lv_local_file) = sender->is_local( ).

    DATA(lt_params) = VALUE btc_t_templ_var(
      ( selname = 'P_BP'     kind = gif_const=>gc_kind-params low = p_dp )
      ( selname = 'P_STCCS'  kind = gif_const=>gc_kind-params low = p_stccs )
      ( selname = 'P_PATH'   kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_true THEN ev_full ELSE space ) )
      ( selname = 'P_PATH2'  kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_false THEN ev_full ELSE space ) )
      ( selname = 'P_PCKGC'  kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_NSCUP'  kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_SIMUL'  kind = gif_const=>gc_kind-params low = abap_false )
    ).

    process_file(
      ir_sender  = sender
      iv_file    = ev_file
      iv_full    = ev_full
      iv_program = gif_const=>gc_program-upload_101
      it_params  = lt_params
    ).
  ENDMETHOD.
ENDCLASS.


CLASS gcl_num_upload_102 IMPLEMENTATION.
  METHOD on_file_found.
    DATA(lv_local_file) = sender->is_local( ).

    DATA(lt_params) = VALUE btc_t_templ_var(
      ( selname = 'P_BP'     kind = gif_const=>gc_kind-params low = p_dp )
      ( selname = 'P_STCCS'  kind = gif_const=>gc_kind-params low = p_stcc2 )
      ( selname = 'P_PATH'   kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_true  THEN ev_full ELSE space ) )
      ( selname = 'P_PATH2'  kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_false THEN ev_full ELSE space ) )
      ( selname = 'P_PCKGC'  kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_NSCUP'  kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_SIMUL'  kind = gif_const=>gc_kind-params low = abap_false )
    ).

    process_file(
      ir_sender  = sender
      iv_file    = ev_file
      iv_full    = ev_full
      iv_program = gif_const=>gc_program-upload_102
      it_params  = lt_params
    ).
  ENDMETHOD.
ENDCLASS.


CLASS gcl_retarif_xml_cus IMPLEMENTATION.
  METHOD on_file_found.
    DATA(lv_local_file) = sender->is_local( ).

    DATA(lt_params) = VALUE btc_t_templ_var(
      ( selname = 'P_BPAR'  kind = gif_const=>gc_kind-params low = p_dp )
      ( selname = 'P_STCTS' kind = gif_const=>gc_kind-params low = p_stcts )
      ( selname = 'P_CTSTY' kind = gif_const=>gc_kind-params low = p_ctsty )
      ( selname = 'P_FPATH' kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_true  THEN ev_full ELSE space ) )
      ( selname = 'P_APATH' kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_false THEN ev_full ELSE space ) )
      ( selname = 'P_APPLS' kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_SFILE' kind = gif_const=>gc_kind-params low = abap_true )
    ).

    process_file(
      ir_sender  = sender
      iv_file    = ev_file
      iv_full    = ev_full
      iv_program = gif_const=>gc_program-retarif
      it_params  = lt_params
    ).
  ENDMETHOD.
ENDCLASS.


CLASS gcl_spl_upload_ixml IMPLEMENTATION.
  METHOD on_file_found.
    DATA(lv_local_file) = sender->is_local( ).

    DATA(lt_params) = VALUE btc_t_templ_var(
      ( selname = 'P_PROVI' kind = gif_const=>gc_kind-params low = p_dp )
      ( selname = 'P_LGREG' kind = gif_const=>gc_kind-params low = p_lgreg )
      ( selname = 'P_PATH'  kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_true  THEN ev_full ELSE space ) )
      ( selname = 'P_PATH2' kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_false THEN ev_full ELSE space ) )
      ( selname = 'P_APPLS' kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_SFILE' kind = gif_const=>gc_kind-params low = abap_true )
    ).
    lt_params = VALUE #( BASE lt_params FOR ls_ltype IN s_ltype[] ( selname = 'S_LTYPE' kind = gif_const=>gc_kind-selopt
      sign   = ls_ltype-sign
      option = ls_ltype-option
      low    = ls_ltype-low
      high   = ls_ltype-high )
    ).
    lt_params = VALUE #( BASE lt_params FOR ls_spgrp IN s_spgrp[] ( selname = 'S_SPGRP' kind = gif_const=>gc_kind-selopt
      sign   = ls_spgrp-sign
      option = ls_spgrp-option
      low    = ls_spgrp-low
      high   = ls_spgrp-high )
    ).

    process_file(
      ir_sender  = sender
      iv_file    = ev_file
      iv_full    = ev_full
      iv_program = gif_const=>gc_program-spl
      it_params  = lt_params
    ).
  ENDMETHOD.
ENDCLASS.


CLASS gcl_xml_measures_upl IMPLEMENTATION.
  METHOD on_file_found.
    DATA(lv_local_file) = sender->is_local( ).

    DATA(lt_params) = VALUE btc_t_templ_var(
      ( selname = 'P_DP'    kind = gif_const=>gc_kind-params low = p_dp )
      ( selname = 'P_STCSM' kind = gif_const=>gc_kind-params low = ev_file+0(2) )
      ( selname = 'P_PATH1' kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_true  THEN ev_full ELSE space ) )
      ( selname = 'P_PATH2' kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_false THEN ev_full ELSE space ) )
      ( selname = 'P_CNTGR' kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_MART'  kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_ZCO'   kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_ZCO'   kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_OLI'   kind = gif_const=>gc_kind-params low = abap_false )
    ).

    process_file(
      ir_sender  = sender
      iv_file    = ev_file
      iv_full    = ev_full
      iv_program = gif_const=>gc_program-measures
      it_params  = lt_params
    ).
  ENDMETHOD.
ENDCLASS.



*
*
*CLASS gcl_type_abstr IMPLEMENTATION.
*  METHOD constructor.
*    " Keep selection data
*    mv_data_provider = iv_data_provider.
*    mr_mode          = ir_mode.
*    mv_program       = iv_program.
*  ENDMETHOD.
*
*  METHOD on_file_found.
*
*    IF mr_mode->is_productive( ) = abap_true.
*      " Move the yet processed file to archive folder
*      TRY.
*          DATA(lr_filehandler) = CAST gif_filehandler( sender ).
*
*          lr_filehandler->move_file_to_archive(
*            iv_file = ev_file
*            iv_path = ev_path
*            iv_full = ev_full ).
*
*        CATCH cx_sy_assign_cast_error.
*          " Should never occur
*          CHECK 1 = 1.
*
*      ENDTRY.
*    ENDIF.
*
*    WRITE:/ |Upload von Datei { ev_file }|.
*
*  ENDMETHOD.
*ENDCLASS.
*
*
*CLASS gcl_tariff IMPLEMENTATION.
*  METHOD constructor.
*    super->constructor(
*      ir_mode          = ir_mode
*      iv_data_provider = iv_data_provider
*      iv_program       = '/SAPSLL/NSC_NUM_UPLOAD_102' ).
*
*    mv_prog   = iv_prog.
*    mv_stccs  = iv_stccs.
*    mt_spras  = it_spras.
*    IF mv_stccs IS INITIAL.
*      " Füllen Sie alle Mussfelder aus
*      MESSAGE e055(00).
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD on_file_found.
*    DATA(lt_params) = VALUE btc_t_templ_var(
*      ( selname = 'P_BP'     kind = gif_mode=>gc_kind-params low = mv_data_provider )
*      ( selname = 'P_STCCS'  kind = gif_mode=>gc_kind-params low = mv_stccs )
*      ( selname = 'P_PATH'   kind = gif_mode=>gc_kind-params low = SWITCH #( sender->is_local( )
*                                                WHEN abap_true THEN ev_full ELSE space ) )
*      ( selname = 'P_PATH2'  kind = gif_mode=>gc_kind-params low = SWITCH #( sender->is_local( )
*                                                WHEN abap_false THEN ev_full ELSE space ) )
*      ( selname = 'P_PCKGC'  kind = gif_mode=>gc_kind-params low = abap_true )
*      ( selname = 'P_NSCUP'  kind = gif_mode=>gc_kind-params low = abap_true )
*      ( selname = 'P_SIMUL'  kind = gif_mode=>gc_kind-params low = abap_false )
*    ).
*
*    lt_params = VALUE #( BASE lt_params FOR ls_spras IN mt_spras ( selname = 'S_LANGU' kind = gif_mode=>gc_kind-selopt
*      sign   = ls_spras-sign
*      option = ls_spras-option
*      low    = ls_spras-low
*      high   = ls_spras-high ) ).
*
*    IF mr_mode->is_productive( ) = abap_true.
*      " Submit the report for uploading a single tariff code file
*      SUBMIT (mv_prog) WITH SELECTION-TABLE lt_params AND RETURN.
*    ENDIF.
*
*    " Move file to archive and output log message
*    super->on_file_found(
*      sender  = sender
*      ev_file = ev_file
*      ev_path = ev_path
*      ev_full = ev_full ).
*
*  ENDMETHOD.
*ENDCLASS.
*
*
*CLASS gcl_control_class IMPLEMENTATION.
*  METHOD constructor.
*    super->constructor(
*      ir_mode          = ir_mode
*      iv_data_provider = iv_data_provider
*      iv_stccs         = iv_stccs
*      it_spras         = it_spras
*      iv_program       = '/SAPSLL/NSC_NUM_UPLOAD_002' ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*CLASS gcl_spl IMPLEMENTATION.
*  METHOD constructor.
*    super->constructor(
*      ir_mode          = ir_mode
*      iv_data_provider = iv_data_provider ).
*
*    mt_ltype = it_ltype.
*    mt_spgrp = it_spgrp.
*    mv_lgreg = iv_lgreg.
*
*    IF mv_lgreg IS INITIAL.
*      " Füllen Sie alle Mussfelder aus
*      MESSAGE e055(00).
*    ENDIF.
*
*  ENDMETHOD.
*
*  METHOD on_file_found.
*
*    DATA(lt_params) = VALUE btc_t_templ_var(
*      ( selname = 'P_LGREG' kind = gif_mode=>gc_kind-params low = mv_lgreg )
*      ( selname = 'P_PROVI' kind = gif_mode=>gc_kind-params low = mv_data_provider )
*      ( selname = 'P_APPLS' kind = gif_mode=>gc_kind-params low = abap_true )
*      ( selname = 'P_SFILE' kind = gif_mode=>gc_kind-params low = abap_true )
*      ( selname = 'P_PATH'  kind = gif_mode=>gc_kind-params low = SWITCH #( sender->is_local( ) WHEN abap_true  THEN ev_full ELSE space ) )
*      ( selname = 'P_PATH2' kind = gif_mode=>gc_kind-params low = SWITCH #( sender->is_local( ) WHEN abap_false THEN ev_full ELSE space ) )
*    ).
*
*    lt_params = VALUE #( BASE lt_params FOR ls_ltype IN mt_ltype ( selname = 'S_LTYPE' kind = gif_mode=>gc_kind-selopt
*      sign   = ls_ltype-sign
*      option = ls_ltype-option
*      low    = ls_ltype-low
*      high   = ls_ltype-high ) ).
*
*    lt_params = VALUE #( BASE lt_params FOR ls_spgrp IN mt_spgrp ( selname = 'S_SPGRP' kind = gif_mode=>gc_kind-selopt
*      sign   = ls_spgrp-sign
*      option = ls_spgrp-option
*      low    = ls_spgrp-low
*      high   = ls_spgrp-high ) ).
*
*    IF mr_mode->is_productive( ) = abap_true.
*      " Submit the report for uploading a single spl file
*      SUBMIT /sapsll/spl_upload_ixml WITH SELECTION-TABLE lt_params AND RETURN.
*    ENDIF.
*
*    " Move file to archive and output log message
*    super->on_file_found(
*      sender  = sender
*      ev_file = ev_file
*      ev_path = ev_path
*      ev_full = ev_full ).
*
*  ENDMETHOD.
*
*ENDCLASS.
*
*
*CLASS gcl_measures IMPLEMENTATION.
*  METHOD constructor.
*    super->constructor(
*      ir_mode          = ir_mode
*      iv_data_provider = iv_data_provider ).
*
*    mv_stcsm = iv_stcsm.
*    IF mv_stcsm IS INITIAL.
*      " Füllen Sie alle Mussfelder aus
*      MESSAGE e055(00).
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD on_file_found.
*    DATA(lt_params) = VALUE btc_t_templ_var(
*      ( selname = 'P_DP'    kind = gif_mode=>gc_kind-params low = mv_data_provider )
*      ( selname = 'P_STCSM' kind = gif_mode=>gc_kind-params low = mv_stcsm )
*      ( selname = 'P_PATH1' kind = gif_mode=>gc_kind-params low = SWITCH #( sender->is_local( ) WHEN abap_true  THEN ev_full ELSE space ) )
*      ( selname = 'P_PATH2' kind = gif_mode=>gc_kind-params low = SWITCH #( sender->is_local( ) WHEN abap_false THEN ev_full ELSE space ) )
*      ( selname = 'P_CNTGR' kind = gif_mode=>gc_kind-params low = abap_true )
*      ( selname = 'P_MART'  kind = gif_mode=>gc_kind-params low = abap_true )
*      ( selname = 'P_ZCO'   kind = gif_mode=>gc_kind-params low = abap_true )
*      ( selname = 'P_ZCO'   kind = gif_mode=>gc_kind-params low = abap_true )
*      ( selname = 'P_OLI'   kind = gif_mode=>gc_kind-params low = abap_false )
*    ).
*
*    IF mr_mode->is_productive( ) = abap_true.
*      " Submit the report for uploading a single mesaurement file
*      SUBMIT /sapsll/sllns_xml_measures_upl WITH SELECTION-TABLE lt_params AND RETURN.
*    ENDIF.
*
*    " Move file to archive and output log message
*    super->on_file_found(
*      sender  = sender
*      ev_file = ev_file
*      ev_path = ev_path
*      ev_full = ev_full ).
*
*  ENDMETHOD.
*ENDCLASS.
