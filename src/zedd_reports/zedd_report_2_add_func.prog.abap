"! Once the program gets large enough, or when you need to add e.g. some additional functions to toolbar,
"! or actions on double_click/link_click, you can move the processing into it's own class - check zedd_report_2_add_func_rep_cls.
"!
"! Class methods are preferred over FORMs, as they have more type safety and better ergonomics.
REPORT zedd_report_2_add_func.

"---------------------------------------------------------------------
" SELECTION SCREEN
"---------------------------------------------------------------------
TABLES:
  mara.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
SELECT-OPTIONS:
  s_matnr FOR mara-matnr.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
PARAMETERS: p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b02.

"---------------------------------------------------------------------
" REPORT
"---------------------------------------------------------------------
INCLUDE zedd_report_2_add_func_rep_cls.

INITIALIZATION.
  DATA(report) = NEW lcl_report( sy-repid ).
  report->init_user_variant( ).

START-OF-SELECTION.
  report->prepare_report( ).
  report->set_progress_bar( text = TEXT-005 ).
  report->display_data( p_layout ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = report->get_layout_from_f4_selection( ).
