*&---------------------------------------------------------------------*
*& Report ZGTS_DP_UPLOAD_R
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgts_dp_upload_r.

INCLUDE: zgts_dp_upload_top, " TOP-Include
         zgts_dp_upload_cd1, " Local Class Definitions
         zgts_dp_upload_sel, " Selection-Screen
         zgts_dp_upload_ci1, " Local Class Implementations
         zgts_dp_upload_f01. " Local Subroutines

*--------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ldir.
  PERFORM get_filename USING px_loc
                    CHANGING p_ldir.

*--------------------------------------------------------------------*
* AT SELECTION-SCREEN ON BLOCK b03
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK b02.
  PERFORM sel_screen_obligatory_check.

*--------------------------------------------------------------------*
* INITIALIZATION
*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM get_textpool.

*--------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM sel_screen_output.

*--------------------------------------------------------------------*
* START-OF-SELECTION
*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM start.
