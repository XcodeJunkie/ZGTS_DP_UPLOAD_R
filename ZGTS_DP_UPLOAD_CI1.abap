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

    IF p_arch = abap_false.
      RETURN.
    ENDIF.

    WRITE:/ |File { iv_file } has been archived.|.

    IF px_simu = abap_true.
      RETURN.
    ENDIF.

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

    MESSAGE i024(cbc_common) WITH iv_path INTO DATA(lv_dummy).
    RAISE EVENT gif_message_sender~message_occured.

    SORT lt_files BY filename   ASCENDING
                     createdate ASCENDING
                     createtime ASCENDING.

    " Process all files and trigger upload for each
    CASE gif_filehandler~ms_dp_upload-procs.
      WHEN gif_const=>gc_procs-newest.
        DATA(ls_file_process) = REDUCE file_info(
            INIT s_file = VALUE file_info( )
            FOR <s_file> IN lt_files
            NEXT s_file = COND #( LET  v_ver_curr = substring_from( val  = <s_file>-filename pcre = gif_const=>pcre_delimiter )
                                       v_ver_high = substring_from( val  = s_file-filename   pcre = gif_const=>pcre_delimiter ) IN
                                  WHEN v_ver_curr > v_ver_high THEN <s_file>
                                  ELSE s_file ) ).
        IF raise_when_matches(
          is_file = ls_file_process
          iv_path = iv_path ) = abap_true.

          LOOP AT lt_files INTO DATA(ls_file).
            gif_filehandler~move_file_to_archive(
              iv_file = ls_file-filename
              iv_full = |{ iv_path }{ gif_filehandler~mv_path_sep }{ ls_file-filename }| ).
          ENDLOOP.
        ENDIF.

      WHEN OTHERS.
        " Take all files
        LOOP AT lt_files INTO ls_file.
          IF raise_when_matches(
            is_file = ls_file
            iv_path = iv_path ) = abap_true.

            gif_filehandler~move_file_to_archive(
              iv_file = ls_file-filename
              iv_full = |{ iv_path }{ gif_filehandler~mv_path_sep }{ ls_file-filename }| ).
          ENDIF.
        ENDLOOP.
    ENDCASE.

  ENDMETHOD.

  METHOD raise_when_matches.
    rv_has_matched = abap_false.

    IF matches( val  = is_file-filename
                pcre = gif_filehandler~ms_dp_upload-regex ).
      rv_has_matched = abap_true.

      RAISE EVENT gif_filehandler~file_found
        EXPORTING ev_file = is_file-filename
                  ev_full = |{ iv_path }{ gif_filehandler~mv_path_sep }{ is_file-filename }|
                  ev_path = iv_path.
    ENDIF.
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

    " Construct scan paths
    DATA(lt_paths) = VALUE tt_paths( ( path = iv_path ) ).

    " Construct exclude paths
    DATA(lt_exclude_paths) = VALUE tt_r_paths( (
      sign   = zif_gts_const=>gc_range-sign-inclusive
      option = zif_gts_const=>gc_range-option-equal
      low    = gif_const=>gc_path-archive
    ) ).

    DO.
      IF lines( lt_paths ) = 0.
        EXIT.
      ENDIF.

      scan_directory(
        EXPORTING it_exclude_paths = lt_exclude_paths
         CHANGING ct_paths         = lt_paths
                   ct_files        = lt_files ).
    ENDDO.

    SORT lt_files BY path ASCENDING
                     name ASCENDING.

    LOOP AT lt_files INTO DATA(ls_file)
                      GROUP BY ( content = ls_file-content )
           REFERENCE INTO DATA(lr_file_grp).
      CASE gif_filehandler~ms_dp_upload-procs.
        WHEN gif_const=>gc_procs-newest.
          DATA(ls_file_process) = REDUCE ts_file(
              INIT s_file = VALUE ts_file( )
              FOR <s_file> IN GROUP lr_file_grp
              NEXT s_file = COND #( LET  v_ver_curr = substring_from( val  = <s_file>-name pcre = gif_const=>pcre_delimiter )
                                         v_ver_high = substring_from( val  = s_file-name   pcre = gif_const=>pcre_delimiter ) IN
                                    WHEN v_ver_curr > v_ver_high THEN <s_file>
                                    ELSE s_file ) ).
          IF raise_when_matches( ls_file_process ) = abap_true.
            LOOP AT GROUP lr_file_grp INTO ls_file.
              gif_filehandler~move_file_to_archive(
                iv_file = ls_file-name
                iv_full = ls_file-full ).
            ENDLOOP.
          ENDIF.

        WHEN OTHERS.
          " Take all files
          LOOP AT GROUP lr_file_grp INTO ls_file.
            IF raise_when_matches( ls_file ) = abap_true.
              gif_filehandler~move_file_to_archive(
                iv_file = ls_file-name
                iv_full = ls_file-full ).
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.

    RAISE EVENT gif_filehandler~upload_finished.

  ENDMETHOD.

  METHOD raise_when_matches.
    rv_has_matched = abap_false.

    IF matches( val  = is_file-name
                pcre = gif_filehandler~ms_dp_upload-regex ).
      rv_has_matched = abap_true.

      RAISE EVENT gif_filehandler~file_found
        EXPORTING ev_file = is_file-name
                  ev_full = is_file-full
                  ev_path = is_file-path.
    ENDIF.
  ENDMETHOD.

  METHOD gif_filehandler~move_file_to_archive.

    TYPES: tt_path TYPE STANDARD TABLE OF /sapsll/path_local_appserver WITH EMPTY KEY.

    DATA: lv_target TYPE /sapsll/path_local_appserver,
          ls_line   TYPE LINE OF tt_lines.

    IF p_arch = abap_false.
      RETURN.
    ENDIF.

    WRITE:/ |File { iv_file } has been archived.|.

    IF px_simu = abap_true.
      RETURN.
    ENDIF.

    " Just to be sure
    CLOSE DATASET iv_full.

    " Open source file for read
    OPEN DATASET iv_full FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Construct target archive path
    DATA(lt_path) = VALUE tt_path( ( gif_const=>gc_path-main )
                                   ( gif_const=>gc_path-archive )
                                   ( iv_file ) ).

    lv_target = gif_filehandler~mv_path_sep && concat_lines_of( table = lt_path
                                                                sep   = gif_filehandler~mv_path_sep ).

    " Open target file for write
    CLOSE DATASET lv_target.
    OPEN DATASET lv_target FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      CLOSE DATASET iv_full.
      RETURN.
    ENDIF.

    DO.
      CLEAR: ls_line.
      READ DATASET iv_full INTO ls_line.
      IF sy-subrc = 0.
        TRANSFER ls_line TO lv_target.

      ELSEIF sy-subrc = 8.
        EXIT.

      ELSE.
        TRANSFER ls_line TO lv_target.
        EXIT.

      ENDIF.
    ENDDO.

    CLOSE DATASET iv_full.
    CLOSE DATASET lv_target.

    DELETE DATASET iv_full.

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
                                                          pcre = '^\.+')
                                         AND NOT ls_file-name IN it_exclude_paths.
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
    CLEAR mt_files.
  ENDMETHOD.

  METHOD on_file_found.
    APPEND VALUE tline(
      tdformat = 'B1'
      tdline   = ev_file
    ) TO mt_files.
  ENDMETHOD.

  METHOD on_upload_finished.
    " Send only email when flag is set
    IF px_email = abap_false.
      RETURN.
    ENDIF.

    " Mail shall only be sent, when email recipients are maintained
    IF s_email[] IS INITIAL.
      RETURN.
    ENDIF.

    " No email when no file has been uploaded
    IF mt_files IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        mr_mail_request = cl_bcs=>create_persistent( ).

        " Build E-Mail body as HTML text
        DATA(lt_mail_body) = build_body_html( ).

        " Mail subject
        DATA(lv_mail_subject) = CONV so_obj_des( TEXT-001 ).

        DATA(lr_document) = cl_document_bcs=>create_document(
          i_type    = 'HTM'
          i_text    = lt_mail_body
          i_subject = lv_mail_subject ).

        mr_mail_request->set_document( lr_document ).

        DATA(lr_sender) = cl_cam_address_bcs=>create_internet_address( i_address_string = 'do-not-reply@eos.info' ).
        mr_mail_request->set_sender( lr_sender ).

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

        " Email konnte nicht versendet werden
        MESSAGE s113(/sapsll/con_lmgm) DISPLAY LIKE 'E'.

    ENDTRY.
  ENDMETHOD.

  METHOD build_body_html.

    " Local declarations
    DATA: ls_thead     TYPE thead,
          lt_mail_itf  TYPE STANDARD TABLE OF tline WITH EMPTY KEY,
          lt_mail_html TYPE STANDARD TABLE OF htmlline WITH EMPTY KEY.

    " Initialization
    CLEAR: rt_email,
           ls_thead,
           lt_mail_itf,
           lt_mail_html.

    TRY.
        " Get SO10 Mail text
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            client                  = sy-mandt
            language                = sy-langu
            id                      = 'ST'
            object                  = 'TEXT'
            name                    = 'Z_REGUVIS_UPLOAD_MAIL'
          IMPORTING
            header                  = ls_thead
          TABLES
            lines                   = lt_mail_itf
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_gts_check.
        ENDIF.

        " Replace run mode variable in mail text
        DATA(lv_mode) = SWITCH string( px_simu WHEN abap_true THEN TEXT-sim ELSE TEXT-pro ).
        CALL FUNCTION 'TEXT_SYMBOL_SETVALUE'
          EXPORTING
            name  = 'LV_MODE'
            value = lv_mode.

        " Replace system id variable in mail text
        CALL FUNCTION 'TEXT_SYMBOL_SETVALUE'
          EXPORTING
            name  = 'LV_SYSID'
            value = sy-sysid.

        " Replace content variable in mail text
        CALL FUNCTION 'TEXT_SYMBOL_SETVALUE'
          EXPORTING
            name  = 'LV_CONTENT'
            value = p_cont.

        " Perform variable substitution
        CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
          EXPORTING
            header       = ls_thead
            replace_text = abap_true
          TABLES
            lines        = lt_mail_itf.

        " Add file names
        APPEND LINES OF mt_files TO lt_mail_itf.

        " Convert SAPScript to HTML
        CALL FUNCTION 'CONVERT_ITF_TO_HTML'
          EXPORTING
            i_header       = ls_thead
          TABLES
            t_itf_text     = lt_mail_itf
            t_html_text    = lt_mail_html
          EXCEPTIONS
            syntax_check   = 1
            replace        = 2
            illegal_header = 3
            OTHERS         = 4.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_gts_check.
        ENDIF.

        " Get HTML as simple string
        DATA(lv_string) = REDUCE string( INIT v_str = ``
                                         FOR  s_row IN lt_mail_html
                                         NEXT v_str = |{ v_str }{ s_row-tdline }| ).
      CATCH zcx_gts_check.
        lv_string = '<html>'.

    ENDTRY.

    " Convert string to SOLI_TAB
    rt_email = cl_document_bcs=>string_to_soli( lv_string ).

  ENDMETHOD.
ENDCLASS.


CLASS gcl_content_abstr IMPLEMENTATION.
  METHOD process_file.
    WRITE:/ |Upload von Datei { iv_file }|.

    IF px_simu = abap_false.
      SUBMIT (iv_program) WITH SELECTION-TABLE it_params AND RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS gcl_num_upload_002 IMPLEMENTATION.
  METHOD on_file_found.
    DATA(lv_local_file) = sender->is_local( ).

    DATA(lt_params) = VALUE btc_t_templ_var(
      ( selname = 'P_BP'     kind = gif_const=>gc_kind-params low = p_dp )
      ( selname = 'P_STCCS'  kind = gif_const=>gc_kind-params low = p_stcc3 )
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
      ( selname = 'P_STCCS'  kind = gif_const=>gc_kind-params low = p_stcc1 )
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
      ( selname = 'P_PROVI'  kind = gif_const=>gc_kind-params low = p_dp )
      ( selname = 'P_LGREG'  kind = gif_const=>gc_kind-params low = p_lgreg )
      ( selname = 'P_PATH'   kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_true  THEN ev_full ELSE space ) )
      ( selname = 'P_PATH2'  kind = gif_const=>gc_kind-params low = SWITCH #( lv_local_file WHEN abap_false THEN ev_full ELSE space ) )
      ( selname = 'P_ENH'    kind = gif_const=>gc_kind-params low = p_enh )
      ( selname = 'P_APPLS'  kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_SFILE'  kind = gif_const=>gc_kind-params low = abap_true )
      ( selname = 'P_DELSUB' kind = gif_const=>gc_kind-params low = p_delsub )
      ( selname = 'P_IGNORE' kind = gif_const=>gc_kind-params low = p_ignore )
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
