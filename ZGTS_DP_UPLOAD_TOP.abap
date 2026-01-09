*&---------------------------------------------------------------------*
*& Include          ZGTS_DP_UPLOAD_TOP
*&---------------------------------------------------------------------*
DATA: gt_textpool  TYPE STANDARD TABLE OF textpool WITH DEFAULT KEY,
      gs_comm_data TYPE suid_st_node_comm_data.

TABLES: /sapsll/t608l,
        /sapsll/t608g,
        sscrfields.
