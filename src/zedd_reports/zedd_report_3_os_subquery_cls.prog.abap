"! In this example subquery class is made generic.
"! An alternative would be to define interface/base class per subquery
"! and swap those based on which function user clicked.
"! In that case you would need to take care of destroying container on swap - you could also manage it in LCL_REPORT class.
CLASS lcl_subquery DEFINITION INHERITING FROM zcl_ea_salv_table CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_tables,
        deliveries TYPE tabname VALUE 'ZEDD_REPORT_3_SUBQUERY_DEL',
        all_docs   TYPE  tabname VALUE 'ZEDD_REPORT_3_SUBQUERY_DOCS',
      END OF c_tables.

    TYPES:
      BEGIN OF t_so,
        sales_order     TYPE vbeln,
        sales_order_pos TYPE posnr,
      END OF t_so,
      tt_so TYPE STANDARD TABLE OF t_so WITH EMPTY KEY.

    METHODS:
      display_table IMPORTING layout_handle TYPE slis_handl table_name TYPE tabname sales_orders TYPE tt_so.

  PROTECTED SECTION.
    METHODS:
      on_link_click REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      fetch_data IMPORTING sales_orders TYPE tt_so,
      prepare_columns.

    DATA:
      current_table TYPE tabname,
      data_table    TYPE REF TO data,
      container     TYPE REF TO cl_gui_docking_container.
ENDCLASS.

CLASS lcl_subquery IMPLEMENTATION.
  METHOD display_table.
    current_table = table_name.
    layout_key-handle = layout_handle.

    "So you could display it fullscreen or on the same screen.
    IF p_new_s = abap_false.
      IF container IS BOUND.
        container->free( EXCEPTIONS OTHERS = 1 ).
      ENDIF.
      container = NEW #( ratio = 50 side = cl_gui_docking_container=>dock_at_bottom ).
      set_container( container ).

    ELSE.
      me->initialise_alv( ). "So layout key is refreshed
    ENDIF.

    fetch_data( sales_orders ).
    prepare_columns( ).
    display_data( p_layout ).
  ENDMETHOD.

  METHOD fetch_data.
    FIELD-SYMBOLS: <table> TYPE table.

    CREATE DATA data_table TYPE TABLE OF (current_table).
    ASSIGN data_table->* TO <table>.

    DATA(where) = |SALES_ORDER = @SALES_ORDERS-SALES_ORDER AND SALES_ORDER_POS = @SALES_ORDERS-SALES_ORDER_POS|.

    SELECT * FROM (current_table)
    FOR ALL ENTRIES IN @sales_orders
    WHERE (where)
    INTO CORRESPONDING FIELDS OF TABLE @<table>.

    set_data( data_table ).
  ENDMETHOD.

  METHOD prepare_columns.
    CASE current_table.
      WHEN c_tables-deliveries.
        columns->set_as_hotspot( 'VBELN' ).
    ENDCASE.
  ENDMETHOD.

  METHOD on_link_click.
    IF row = 0. RETURN. ENDIF.

    FIELD-SYMBOLS <table> TYPE table.
    ASSIGN me->data_table_ref->* TO <table>.
    ASSIGN COMPONENT column OF STRUCTURE <table>[ row ] TO FIELD-SYMBOL(<cell_value>).

    "Display on click, but in another window, so you could still see current report
    DATA: spagpa_tab TYPE STANDARD TABLE OF rfc_spagpa WITH EMPTY KEY.
    DATA: tcode TYPE sy-tcode.

    CASE current_table.
      WHEN c_tables-deliveries.
        CASE column.
          WHEN 'VBELN'.
            tcode = 'VL03N'.
            APPEND VALUE #( parid = 'VL' parval = <cell_value>  ) TO spagpa_tab.
        ENDCASE.

      WHEN OTHERS.
        RETURN.
    ENDCASE.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION' STARTING NEW TASK 'OPEN_TCODE_IN_NEW_WINDOW'
      EXPORTING
        tcode       = tcode
        skip_screen = abap_true
      TABLES
        spagpa_tab  = spagpa_tab.
  ENDMETHOD.
ENDCLASS.
