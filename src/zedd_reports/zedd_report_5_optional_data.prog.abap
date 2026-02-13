"! Sometimes you may find that joins with additional data (e.g. aggregations) are very expensive,
"! because DB will materialize them before executing join.
"! E.g. you want to group deliveries by sales order, but when joining it with sales order table,
"! all deliveries, whole LIPS is grouped, before join, which results in terrible performance.
"! Similar case can happen when you select MAX record out of many and want to join with it.
"!
"! In such cases, you can try to grab additional data by using select-options with key on which you group.
"! It sometimes help and can be more performant than join or FOR ALL ENTRIES,
"! but code is longer than in previous examples, and may actually be slower id db is smart enough with joins,
"! so you should use it only if you encounter slow joins.
"! Always compare and verify (e.g. run ST05, export execution plan (.plv file), and check that it truly is a problem) if it's needed.
"!
"! It's hard to find particular example - in simple cases DB might be able to join before aggregation,
"! but in more complicated, with multiple joins and columns it might not be possible
"! - as always, check trace how the query is executed
REPORT zedd_report_5_optional_data.

"---------------------------------------------------------------------
" SELECTION SCREEN
"---------------------------------------------------------------------
TABLES:
  zedd_report_3b.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
PARAMETERS:
  p_j1 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
SELECT-OPTIONS:
  s_vbeln FOR zedd_report_3b-sales_order.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-s03.
PARAMETERS: p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b03.

"---------------------------------------------------------------------
" REPORT
"---------------------------------------------------------------------
TYPES:
  BEGIN OF t_output.
    INCLUDE TYPE zedd_report_3_base.
  TYPES:
    lfimg TYPE lfimg,
  END OF t_output,
  tt_output TYPE STANDARD TABLE OF t_output WITH EMPTY KEY
    WITH  UNIQUE SORTED KEY so COMPONENTS sales_order sales_order_pos.

INITIALIZATION.
  DATA(report) = NEW zcl_ea_salv_table( sy-repid ).
  report->init_user_variant( ).
  DATA(output) = VALUE tt_output( ).

START-OF-SELECTION.
  SELECT * FROM zedd_report_3_base
  WHERE sales_order IN @s_vbeln
  INTO CORRESPONDING FIELDS OF TABLE @output.

  report->set_data( REF #( output ) ).

  IF p_j1 = abap_false.
    report->columns->set_as_hidden( 'LFIMG' ).

  ELSEIF lines( output ) > 0.
    DATA: so_range TYPE RANGE OF vbeln.
    so_range = VALUE #( FOR <row> IN output ( sign = 'I' option = 'EQ' low = <row>-sales_order ) ).

    TRY.
        "We try to execute with query with range, but it's possible range will be too big,
        "and query will terminate with SAPSQL_STMT_TOO_LARGE.
        "In  that case we fall back to FOR ALL ENTRIES. If it solves the performance issue - great, use it instead of range.
        "But plan execution will probably be similar to the one with JOIN.
        SELECT * FROM zedd_report_3_add_data WHERE sales_order IN @so_range INTO TABLE @DATA(add_info).

      CATCH cx_sy_open_sql_db.
        SELECT * FROM zedd_report_3_add_data
        FOR ALL ENTRIES IN @output
        WHERE sales_order = @output-sales_order
        INTO TABLE @add_info.
    ENDTRY.

    LOOP AT add_info REFERENCE INTO DATA(add_info_row).
      LOOP AT output REFERENCE INTO DATA(row) USING KEY so
      WHERE sales_order = add_info_row->sales_order AND sales_order_pos = add_info_row->sales_order_pos.
        row->lfimg = add_info_row->lfimg.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  report->display_data( p_layout ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = report->get_layout_from_f4_selection( ).
