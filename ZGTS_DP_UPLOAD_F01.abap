*&---------------------------------------------------------------------*
*& Include          ZGTS_DP_UPLOAD_F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form sel_screen_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sel_screen_output.
  CONSTANTS:
    BEGIN OF lc_modif_id,
      local_file  TYPE char3 VALUE 'LOC',
      server_file TYPE char3 VALUE 'SRV',
      datab       TYPE char3 VALUE 'DAT',
      stccs1      TYPE char3 VALUE 'CS1',
      stccs2      TYPE char3 VALUE 'CS2',
      stcts       TYPE char3 VALUE 'CTS',
      ctsty       TYPE char3 VALUE 'CTY',
      langu       TYPE char3 VALUE 'SPR',
      legal_reg   TYPE char3 VALUE 'LGR',
      spl_type    TYPE char3 VALUE 'TYP',
      spl_group   TYPE char3 VALUE 'GRP',
    END OF lc_modif_id.

  " Default Legal regulation for SPL
  p_lgreg = SWITCH #( p_cont
    WHEN gif_const=>gc_content-splde THEN zif_gts_const=>gc_lgreg-zspl0
    WHEN gif_const=>gc_content-splus THEN zif_gts_const=>gc_lgreg-splus
    ELSE space
  ).

  p_datab = SWITCH #( p_cont
    WHEN gif_const=>gc_content-reteu THEN sy-datlo
    WHEN gif_const=>gc_content-retuk THEN sy-datlo
    ELSE space
  ).

  LOOP AT SCREEN INTO DATA(ls_screen).
    " Set field input
    ls_screen-input = SWITCH #( ls_screen-group1
      WHEN lc_modif_id-server_file THEN 0
      ELSE ls_screen-input
    ).
*
    " Set field visibility
    ls_screen-active = SWITCH #( ls_screen-group1
      WHEN lc_modif_id-stccs1 THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-taruk THEN 1
        WHEN gif_const=>gc_content-tarus THEN 1
        ELSE 0 )
      WHEN lc_modif_id-stccs2 THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-alnde THEN 1
        WHEN gif_const=>gc_content-duaeu THEN 1
        WHEN gif_const=>gc_content-cclus THEN 1
        ELSE 0 )
      WHEN lc_modif_id-stcts THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-reteu THEN 1
        WHEN gif_const=>gc_content-retuk THEN 1
        ELSE 0 )
      WHEN lc_modif_id-ctsty THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-reteu THEN 1
        WHEN gif_const=>gc_content-retuk THEN 1
        ELSE 0 )
      WHEN lc_modif_id-datab THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-reteu THEN 1
        WHEN gif_const=>gc_content-retuk THEN 1
        ELSE 0 )
      WHEN lc_modif_id-langu THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-alnde THEN 1
        WHEN gif_const=>gc_content-duaeu THEN 1
        WHEN gif_const=>gc_content-cclus THEN 1
        ELSE 0 )
      WHEN lc_modif_id-legal_reg THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-splde THEN 1
        WHEN gif_const=>gc_content-splus THEN 1
        ELSE 0 )
      WHEN lc_modif_id-spl_type THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-splde THEN 1
        WHEN gif_const=>gc_content-splus THEN 1
        ELSE 0 )
      WHEN lc_modif_id-spl_group THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-splde THEN 1
        WHEN gif_const=>gc_content-splus THEN 1
        ELSE 0 )
      WHEN lc_modif_id-local_file THEN SWITCH #( px_loc
        WHEN abap_true THEN 1
        ELSE 0 )
      WHEN lc_modif_id-server_file THEN SWITCH #( px_srv
        WHEN abap_true THEN 1
        ELSE 0 )
      ELSE ls_screen-active
    ).

    " Set field requirement
    ls_screen-required = SWITCH #( ls_screen-group1
      WHEN lc_modif_id-local_file THEN SWITCH #( px_loc WHEN abap_true THEN 2 ELSE 0 )
      WHEN lc_modif_id-legal_reg THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-splde THEN 2
        WHEN gif_const=>gc_content-splus THEN 2
        ELSE 0 )
      WHEN lc_modif_id-stccs2 THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-alnde THEN 2
        WHEN gif_const=>gc_content-duaeu THEN 2
        WHEN gif_const=>gc_content-cclus THEN 2
        ELSE 0 )
      WHEN lc_modif_id-stcts THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-reteu THEN 2
        WHEN gif_const=>gc_content-retuk THEN 2
        ELSE 0 )
      WHEN lc_modif_id-datab THEN SWITCH #( p_cont
        WHEN gif_const=>gc_content-reteu THEN 2
        WHEN gif_const=>gc_content-retuk THEN 2
        ELSE 0 )
      ELSE ls_screen-required
    ).
    MODIFY SCREEN FROM ls_screen.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form start
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM start.

  " Local declarations
  DATA: lr_file      TYPE REF TO gif_filehandler,
        lr_content   TYPE REF TO gcl_content_abstr,
        ls_dp_upload TYPE zgts_dp_upload.

  CLEAR ls_dp_upload.
  CALL FUNCTION 'ZGTS_DP_UPLOAD_DB_SGL_READ'
    EXPORTING
      is_pk        = VALUE zgts_dp_upload_pk_s(
                       mandt   = sy-mandt
                       content = p_cont )
      is_debug     = VALUE /sapsll/debug_s( )
    IMPORTING
      es_result    = ls_dp_upload
    EXCEPTIONS
      no_data      = 1
      invalid_call = 2
      OTHERS       = 3.
  IF sy-subrc <> 0.
  ENDIF.

  " File handler
  lr_file = SWITCH #( px_loc
    WHEN abap_true THEN NEW gcl_file_local( ls_dp_upload )
    ELSE                NEW gcl_file_server( ls_dp_upload ) ).

  DATA(lr_email) = NEW gcl_email_handler( s_email[] ).

  " Which is the correct file path (local/server)
  DATA(lv_dir) = SWITCH #( px_loc
    WHEN abap_true THEN p_ldir
    ELSE                lr_file->mv_path_sep && p_sdir ).

  " Which data type is handled in this run?
  lr_content = SWITCH #( p_cont
    WHEN gif_const=>gc_content-alnde THEN NEW gcl_num_upload_002( )
    WHEN gif_const=>gc_content-cclus THEN NEW gcl_num_upload_002( )
    WHEN gif_const=>gc_content-duaeu THEN NEW gcl_num_upload_002( )
    WHEN gif_const=>gc_content-meade THEN NEW gcl_xml_measures_upl( )
    WHEN gif_const=>gc_content-meaeu THEN NEW gcl_xml_measures_upl( )
    WHEN gif_const=>gc_content-splde THEN NEW gcl_spl_upload_ixml( )
    WHEN gif_const=>gc_content-splus THEN NEW gcl_spl_upload_ixml( )
    WHEN gif_const=>gc_content-tareu THEN NEW gcl_num_upload_102( )
    WHEN gif_const=>gc_content-taruk THEN NEW gcl_num_upload_101( )
    WHEN gif_const=>gc_content-tarus THEN NEW gcl_num_upload_101( )
    WHEN gif_const=>gc_content-reteu THEN NEW gcl_retarif_xml_cus( )
    WHEN gif_const=>gc_content-retuk THEN NEW gcl_retarif_xml_cus( )
  ).

  " Event handler for main content
  SET HANDLER lr_content->on_file_found FOR lr_file.

  " Event handler for email service
  SET HANDLER lr_email->on_file_found      FOR lr_file.
  SET HANDLER lr_email->on_upload_finished FOR lr_file.
*
  " Start data upload
  lr_file->start_upload( lv_dir ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_filename
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FILE
*&---------------------------------------------------------------------*
FORM get_filename USING iv_flag_local TYPE abap_bool
               CHANGING cv_path       TYPE string.

  " Open browser pop-up to select upload directory
  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      selected_folder      = cv_path
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    CLEAR cv_path.
    MESSAGE e001(pc).
    RETURN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_TEXTPOOL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_textpool.
  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname           = sy-cprog
      action               = 'READ'
    TABLES
      tpool                = gt_textpool
    EXCEPTIONS
      object_not_found     = 1
      permission_failure   = 2
      invalid_program_type = 3
      error_occured        = 4
      action_cancelled     = 5
      OTHERS               = 6.
  IF sy-subrc <> 0.
    CLEAR gt_textpool.
    RETURN.
  ENDIF.

  LOOP AT gt_textpool ASSIGNING FIELD-SYMBOL(<ls_textpool>) WHERE id = 'S'.
    <ls_textpool>-entry = <ls_textpool>-entry+8.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SEL_SCREEN_OBLIGATORY_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sel_screen_obligatory_check .
  IF sscrfields-ucomm <> 'ONLI'.
    RETURN.
  ENDIF.

  LOOP AT SCREEN INTO DATA(ls_screen).
    IF ls_screen-active = 1 AND ls_screen-required = 2.
      ASSIGN (ls_screen-name) TO FIELD-SYMBOL(<lv_field>).
      IF sy-subrc = 0 AND <lv_field> IS INITIAL.
        TRY.
            MESSAGE e278(00) WITH gt_textpool[ key = ls_screen-name ]-entry.
          CATCH cx_sy_itab_line_not_found.
            MESSAGE e055(00).
        ENDTRY.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
