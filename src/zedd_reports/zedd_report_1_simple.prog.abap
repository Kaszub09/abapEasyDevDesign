"! When creating a new report, start with the simplest code - just grab data from DB and display,
"! no need for complicated structure, multiple includes etc.
"!
"! Once you feel that the program is getting too large and would benefit from breaking it down into smaller chunks,
"! fell free to do it - take a look at next examples.
"!
"!
"! ========== How to approach the problem: ==========
"! 1. Start with identifying the base table (e.g. for delivery positions it's LIPS, for material per warehouse MARD etc.),
"! which will be on the same level as the resulting report
"!
"! 2. Join table with all necessary data.
"! 2.1 Prefer INNER join where it makes sense (e.g. master data for material in delivery, or header+positions),
"! but use LEFT JOIN otherwise (e.g. text for material - could exist in one language but not in the other
"! 2.2 In general, try to grab everything you need in one query - it's the simplest and most performant method.
"! 2.3 In general, CDS are preferred, since they provide more functions than OpenSQL, and could be reused.
"! 2.4 Take care with joins to avoid duplicates - preferably always join of full key
"! 2.5 If you need to aggregate data at another level, you can create subquery CDS with grouped data and then join it.
"! But monitor performance in case SQL engine aggregates whole table before joining with base table.
"!
"! 3. Display data.
REPORT zedd_report_1_simple.

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
  DATA(report) = NEW zcl_ea_salv_table( sy-repid ).
  report->init_user_variant( ).

START-OF-SELECTION.
  SELECT * FROM zedd_report_1
  WHERE matnr IN @s_matnr
  INTO TABLE @DATA(data_table). "Becomes global variable, lives as long as program

  report->set_data( REF #( data_table ) ).
  report->display_data( p_layout ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = report->get_layout_from_f4_selection( ).
