"! Let's assume you have sales order report, and want to show all deliveries for given SO.
"! The problem is that this could potentially duplicate data, which could mess up calculations,
"! if, e.g. user wants to sum all rows in SO and such.
"!
"! One solution is to make this data available as additional subquery, displayed on demand.
REPORT zedd_report_3_optional_subq.

"---------------------------------------------------------------------
" SELECTION SCREEN
"---------------------------------------------------------------------
TABLES:
  vbap.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
SELECT-OPTIONS:
  s_vbeln FOR vbap-vbeln.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
PARAMETERS:
  p_new_s  AS CHECKBOX DEFAULT abap_false,
  p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b02.

"---------------------------------------------------------------------
" REPORT
"---------------------------------------------------------------------
INCLUDE zedd_report_3_os_subquery_cls.
INCLUDE zedd_report_3_os_report_cls.

INITIALIZATION.
  DATA(report) = NEW lcl_report( sy-repid ).
  report->init_user_variant( ).

START-OF-SELECTION.
  report->prepare_report( ).
  report->display_data( p_layout ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = report->get_layout_from_f4_selection( ).
