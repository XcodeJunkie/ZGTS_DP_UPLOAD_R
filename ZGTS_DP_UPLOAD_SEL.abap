*&---------------------------------------------------------------------*
*& Include          ZGTS_DP_UPLOAD_SEL
*&---------------------------------------------------------------------*0
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.

  PARAMETERS: p_dp   TYPE /sapsll/bpdpv
       MATCHCODE OBJECT /sapsll/bp_data_provider
              MEMORY ID /sapsll/bp
             OBLIGATORY.
  SELECTION-SCREEN COMMENT 52(50) gv_dpnam.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
  PARAMETERS: p_cont TYPE zgts_dp_content
                  DEFAULT gif_const=>gc_content-alnde
               OBLIGATORY
               AS LISTBOX
           VISIBLE LENGTH 40
             USER-COMMAND cont.

  SELECTION-SCREEN BEGIN OF BLOCK b05 WITH FRAME TITLE TEXT-b05.
    PARAMETERS p_datab TYPE /sapsll/rclsd
                   MODIF ID dat.
    PARAMETERS p_stcc1 TYPE /sapsll/stccs
           MATCHCODE OBJECT /sapsll/llns_stccs_101
                   MODIF ID cs1.
    PARAMETERS p_stcc2 TYPE /sapsll/stccs
           MATCHCODE OBJECT /sapsll/llns_stccs_102
                   MODIF ID cs2.
    PARAMETERS p_stcc3 TYPE /sapsll/stccs
           MATCHCODE OBJECT /sapsll/llns_stccs_002
                   MODIF ID cs3.
    PARAMETERS p_stcts TYPE /sapsll/stcts_n
                   MODIF ID cts.
    PARAMETERS p_ctsty TYPE /sapsll/ctsty_n
                   MODIF ID cty.
    SELECT-OPTIONS: s_langu FOR sy-langu
                   NO INTERVALS
                       MODIF ID spr.
    PARAMETERS p_lgreg TYPE /sapsll/lgreg_n
           MATCHCODE OBJECT /sapsll/shlp_gg_spl
                   MODIF ID lgr.
    SELECT-OPTIONS: s_ltype FOR /sapsll/t608l-spl_list_type
                       MODIF ID typ,
                    s_spgrp FOR /sapsll/t608g-spl_group
                       MODIF ID grp.
    PARAMETERS p_enh TYPE xfeld
                   MODIF ID enh.
    PARAMETERS p_delsub TYPE xfeld
                   MODIF ID del.
    PARAMETERS p_ignore TYPE xfeld
                   MODIF ID ign.
  SELECTION-SCREEN END OF BLOCK b05.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.
  PARAMETERS: px_srv RADIOBUTTON GROUP r1 DEFAULT 'X' USER-COMMAND fil,
              px_loc RADIOBUTTON GROUP r1.
  PARAMETERS: p_ldir TYPE string
               LOWER CASE
                 MODIF ID loc,
              p_sdir TYPE string
               LOWER CASE
                 MODIF ID srv
                  DEFAULT gif_const=>gc_path-main.
  PARAMETERS: p_arch AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b03.


SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE TEXT-b04.
  PARAMETERS: px_email TYPE xfeld DEFAULT abap_true.
  SELECT-OPTIONS: s_email FOR gs_comm_data-smtp_addr NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b04.

PARAMETERS: px_simu TYPE xfeld DEFAULT abap_true.
