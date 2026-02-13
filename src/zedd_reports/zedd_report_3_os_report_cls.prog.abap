"! Display main data, and allow user to display another on demand.
CLASS lcl_report DEFINITION INHERITING FROM zcl_ea_salv_table.
  PUBLIC SECTION.
    METHODS:
      prepare_report.

  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF c_func,
        deliveries TYPE  zcl_ea_salv_table_functions=>t_function VALUE 'DELIVERIES',
        all_docs   TYPE  zcl_ea_salv_table_functions=>t_function VALUE 'ALL_DOCS',
      END OF c_func.

    TYPES:
      tt_output TYPE STANDARD TABLE OF zedd_report_3_base WITH EMPTY KEY.

    METHODS:
      on_added_function REDEFINITION.

    DATA:
      output   TYPE tt_output,
      subquery TYPE REF TO lcl_subquery.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD prepare_report.
    SELECT * FROM zedd_report_3_base WHERE sales_order IN @s_vbeln INTO CORRESPONDING FIELDS OF TABLE @output.

    set_data( REF #( output ) ).

    "You could also skip selections and always make subquery for whole result.
    alv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    "One possibiliy would be to have those functions in some configuration table, would allow to add another without modifying program,
    "but that would be an overkill
    functions->add_function( function = c_func-deliveries description = VALUE #( icon_id = '@4A@' icon_text = TEXT-001 text = TEXT-001 ) ).
    functions->add_function( function = c_func-all_docs description = VALUE #( icon_id = '@GT@' icon_text = TEXT-002 text = TEXT-002 ) ).
  ENDMETHOD.

  METHOD on_added_function.
    DATA(row_selection) = me->alv_table->get_selections( )->get_selected_rows( ).
    IF lines( row_selection ) = 0.
      MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF subquery IS NOT BOUND.
      subquery = NEW #( ).
    ENDIF.

    DATA(sales_orders) = VALUE lcl_subquery=>tt_so( FOR index IN row_selection ( CORRESPONDING #( output[ index ] ) ) ).

    "Handle just need to be different, so you could have same layout name for each subquery,
    "and load it everytime from selection screen parameter 'p_layout'.
    CASE function.
      WHEN c_func-deliveries.
        subquery->display_table( layout_handle = '0001' table_name = subquery->c_tables-deliveries sales_orders = sales_orders ).
      WHEN c_func-all_docs.
        subquery->display_table( layout_handle = '0002' table_name = subquery->c_tables-all_docs sales_orders = sales_orders ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
