"! Structure of program is similar:
"! 1. Grab data (as much as you can in one query)
"! 2. Do additional processing if required
"! 2.1 Add progress bar if you expect long loops, or have several long methods between which you could add it
"! 3. Set table ref
"! 4. Format columns (colors, links, names etc.)
"! 5. Handle events if needed
"!
"! Use TEXT-XXX instead of hardcoding texts, in case you need to translate or change them.
CLASS lcl_report DEFINITION INHERITING FROM zcl_ea_salv_table.
  PUBLIC SECTION.
    METHODS:
      prepare_report.

  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF c_func,
        my_func TYPE  zcl_ea_salv_table_functions=>t_function VALUE 'MY_FUNCTION',
      END OF c_func.

    TYPES:
      "You could also declare TEXT as empty field in CDS zedd_report_1, and fill it there,
      "but this way is cleaner and more logical.
      BEGIN OF t_output.
        INCLUDE TYPE zedd_report_1.
      TYPES:
        text TYPE string,
      END OF t_output,
      tt_output TYPE STANDARD TABLE OF t_output WITH EMPTY KEY
        WITH NON-UNIQUE SORTED KEY matnr COMPONENTS matnr.

    METHODS:
      fill_texts,
      on_link_click REDEFINITION,
      on_added_function REDEFINITION.

    DATA:
        output TYPE tt_output.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD prepare_report.
    set_progress_bar( text = TEXT-003 ).
    "Since selection options are global, and usually not modified after START-OF-SELECTION,
    "it's fine to use them directly. If the class was global, you would need to pass them explicitly as parameter,
    "but I don't see much point in doing so with this kind of report - too much boilerplate.
    SELECT * FROM zedd_report_1
    WHERE matnr IN @s_matnr
    INTO CORRESPONDING FIELDS OF TABLE @output.

    set_progress_bar( text = TEXT-004 ).
    "Additional processing, which couldn't be done in query.
    fill_texts( ).

    "Set data and add columns info.
    set_data( REF #( output ) ).

    "I don't see a point in catching errors, you would want to know if you misspelled column name immediately,
    "and other than that it should always work.
    columns->set_as_hotspot( 'MATNR' ).
    columns->set_color( column = 'MATNR' color = VALUE #( col = 5 ) ).
    columns->set_fixed_text( column = 'TEXT' text = TEXT-c01 ).

    functions->add_function( function = c_func-my_func description = VALUE #( icon_id = '@05@' icon_text = TEXT-001 text = TEXT-001 ) ).
  ENDMETHOD.

  METHOD fill_texts.
    "Usually, you grab some data in batch, and then process per row.
    "In case of long loops you can add progress bar inside it.
    DATA(texts) = NEW zcl_ed_sapscript_text( )->get_texts_tab( objects = VALUE #( ( sign = 'I' option = 'EQ' low = 'MATERIAL' ) )
                            ids = VALUE #( ( sign = 'I' option = 'EQ' low = 'IVER' ) )
                            langs = VALUE #( ( sign = 'I' option = 'EQ' low = sy-langu ) )
                            names_table = output
                            names_col = 'MATNR' ).

    "Try to put smaller table outside and bigger inside - since table inside will be accessed by sorted/hashed key,
    "and bigger tables benefit from it more.
    "(n*log(m) vs m*log(n) - you want the bigger table to be inside log())
    LOOP AT texts REFERENCE INTO DATA(text).
      "Always access internal table by sorted/hashed key. Add secondary if needed.
      "Don't worry about overhead (unless you verified it's a problem), since if table is small, whole report will execute fast anyway.
      LOOP AT output REFERENCE INTO DATA(row) USING KEY matnr WHERE matnr = text->name.
        row->text = text->text.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_link_click.
    IF row = 0. RETURN. ENDIF.

    DATA(row_ref) = REF #( output[ row ] ).

    CASE column.
      WHEN 'MATNR'.
        SET PARAMETER ID 'MAT' FIELD row_ref->matnr.
        CALL TRANSACTION 'MM03' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDMETHOD.

  METHOD on_added_function.
    CASE function.
      WHEN c_func-my_func.
        MESSAGE TEXT-002 TYPE 'I'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

"=================================================================
"-----------------------------------------------------------------
"! Alternative version with inline declaration.
CLASS lcl_report_alt DEFINITION INHERITING FROM zcl_ea_salv_table.
  PUBLIC SECTION.
    METHODS:
      prepare_report.

  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF c_func,
        my_func TYPE  zcl_ea_salv_table_functions=>t_function VALUE 'MY_FUNCTION',
      END OF c_func.

    METHODS:
      on_link_click REDEFINITION,
      on_added_function REDEFINITION.
ENDCLASS.

CLASS lcl_report_alt IMPLEMENTATION.
  METHOD prepare_report.
    SELECT * FROM zedd_report_1
    WHERE matnr IN @s_matnr
    INTO TABLE @DATA(output).

    set_data( create_table_copy = abap_true data_table = REF #( output ) ).

    columns->set_as_hotspot( 'MATNR' ).
    columns->set_color( column = 'MATNR' color = VALUE #( col = 5 ) ).

    functions->add_function( function = c_func-my_func description = VALUE #( icon_id = '@05@' icon_text = TEXT-001 text = TEXT-001 ) ).
  ENDMETHOD.

  METHOD on_link_click.
    IF row = 0. RETURN. ENDIF.

    FIELD-SYMBOLS <table> TYPE table.
    ASSIGN me->data_table_ref->* TO <table>.
    ASSIGN COMPONENT column OF STRUCTURE <table>[ row ] TO FIELD-SYMBOL(<cell_value>).

    CASE column.
      WHEN 'MATNR'.
        SET PARAMETER ID 'MAT' FIELD <cell_value>.
        CALL TRANSACTION 'MM03' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDMETHOD.

  METHOD on_added_function.
    CASE function.
      WHEN c_func-my_func.
        MESSAGE TEXT-002 TYPE 'I'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
