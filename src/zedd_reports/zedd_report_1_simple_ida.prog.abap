"! IDA is an interesting alternative, which could be useful for reports with multiple columns.
"! When columns are hidden, they are no queried for.
"!
"! So e.g. if you have query with 100 columns, and layout saved with only 10, and start report with this layout,
"! only those 10 columns are transferred from DB.
"!
"! On the flipside, any column switching sorting etc. takes place in DB, and if the query is complicated,
"! it could be expensive to rerun every time, and may not handle well the LIMIT/OFFSET used by IDA.
"! And of course requires whole logic inside CDS.
REPORT zedd_report_1_simple_ida.

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

INITIALIZATION.
  DATA(report) = NEW zcl_ea_salv_table( sy-repid ). "Still useful for layout/init variant
  report->init_user_variant( ).

START-OF-SELECTION.
  DATA(ida) = cl_salv_gui_table_ida=>create_for_cds_view( iv_cds_view_name = 'ZEDD_REPORT_1' ).

  "Selection
  DATA(sel) = NEW cl_salv_range_tab_collector( ).
  sel->add_ranges_for_name( iv_name = 'MATNR' it_ranges = s_matnr[] ). "Why not return itself and allow method chaining? :(
  sel->get_collected_ranges( IMPORTING et_named_ranges = DATA(et_named_ranges) ). "Why no returning? :(
  ida->set_select_options( it_ranges = et_named_ranges ).

  "Layout
  ida->layout_persistence( )->set_persistence_options( CORRESPONDING #( report->alv_table->get_layout( )->get_key( )
   MAPPING report_name = report log_group = logical_group ) ).
  TRY.
      ida->layout_persistence( )->set_start_layout( p_layout ).
    CATCH cx_root.
  ENDTRY.

  ida->fullscreen( )->display( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = report->get_layout_from_f4_selection( ).
