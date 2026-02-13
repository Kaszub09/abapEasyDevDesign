"! As per problem in REPORT 3, another solution is to execute join dynamically,
"! based on whatever user select at selection screen.
"!
"! It could be useful when some columns are calculation heavy, so you want to make them optional.
"! It also allows to use selection-options for joined data.
"!
"! You could declare data up front, hide, and fill/show only when needed.
"! But then you also need to manually query db, process it and fill - which would be slower than join,
"! and wouldn't allow to use select options on them.
"!
"! Most of the following code could be copied between reports.
"! You could extend it to hide duplicate fields, color columns etc.
REPORT zedd_report_4_optional_join.

"---------------------------------------------------------------------
" SELECTION SCREEN
"---------------------------------------------------------------------
TABLES:
  zedd_report_3b, zedd_report_3sd,zedd_report_3sdo.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
"Dummy command so AT SELECTION-SCREEN OUTPUT event is raised as soon as checkbox is clicked
PARAMETERS:
  p_j1 AS CHECKBOX USER-COMMAND dummy,
  p_j2 AS CHECKBOX USER-COMMAND dummy,
  p_j3 AS CHECKBOX USER-COMMAND dummy.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
SELECT-OPTIONS:
  s_vbeln FOR zedd_report_3b-sales_order,
  s_matnr FOR zedd_report_3b-matnr.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-s03.
SELECT-OPTIONS:
  s_del FOR zedd_report_3sd-vbeln MODIF ID j1.
SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE TEXT-s04.
SELECT-OPTIONS:
  s_vbtypn FOR zedd_report_3sdo-vbtyp_n MODIF ID j2.
SELECTION-SCREEN END OF BLOCK b04.

"---------------------------------------------------------------------
" REPORT
"---------------------------------------------------------------------

START-OF-SELECTION.
  "Basic structure for optional joins
  TYPES:
    BEGIN OF t_cond_join,
      active      TYPE abap_bool, "Whether to execute join or not
      struct_name TYPE c LENGTH 1, "Struct name in result / alias in query
      struct_type TYPE c LENGTH 30, "Struct type - same as join table
      join        TYPE string, "Join condition
      where       TYPE string, "Where condition
    END OF t_cond_join,
    tt_cond_join TYPE STANDARD TABLE OF t_cond_join WITH EMPTY KEY.

  "Actual join definitions, this is what changes between reports
  DATA(joins) = VALUE tt_cond_join(
  "First row should be base and always active
  "Alias should be same as struct_name, but you could chcange that logic.
      ( active = abap_true struct_name = 'A' struct_type = 'ZEDD_REPORT_3_BASE' join = 'ZEDD_REPORT_3_BASE as A'
        where = 'a~sales_order in @s_vbeln AND a~matnr in @s_matnr' )
  "Now add extensions
     ( active = p_j1 struct_name = 'B' struct_type = 'ZEDD_REPORT_3_SUBQUERY_DEL'
        join = 'LEFT JOIN ZEDD_REPORT_3_SUBQUERY_DEL as B ON b~sales_order = a~sales_order AND b~sales_order_pos = a~sales_order_pos'
       where = 'AND b~vbeln in @s_del' )
      ( active = p_j2 struct_name = 'C' struct_type = 'ZEDD_REPORT_3_SUBQUERY_DOCS'
        join = 'LEFT JOIN ZEDD_REPORT_3_SUBQUERY_DOCS as C ON c~sales_order = a~sales_order AND c~sales_order_pos = a~sales_order_pos')
      ( active = p_j3 struct_name = 'D' struct_type = 'ZEDD_REPORT_3_ADD_DATA'
        join = 'LEFT JOIN ZEDD_REPORT_3_ADD_DATA as D ON d~sales_order = a~sales_order AND d~sales_order_pos = a~sales_order_pos') ).

  "Construct query
  DATA:
    select     TYPE string,
    from       TYPE string,
    where      TYPE string,
    components TYPE cl_abap_structdescr=>component_table.
  LOOP AT joins REFERENCE INTO DATA(join) WHERE active = abap_true.
    select = |{ select } { join->struct_name }~*,|.
    from = |{ from } { join->join }|.
    where = |{ where } { join->where }|.
    APPEND VALUE #( name = join->struct_name type = CAST #( cl_abap_structdescr=>describe_by_name( join->struct_type ) ) ) TO components.
  ENDLOOP.
  select = substring( val = select len = strlen( select ) - 1 ). "Remove last comma

  "Create output table, with following row structure:
  "
  "Types:
  "    begin of t_output,
  "       a type ZEDD_REPORT_3_BASE,
  "       b type ZEDD_REPORT_3_SUBQUERY_DEL,
  "       c type ZEDD_REPORT_3_SUBQUERY_DOCS,
  "   end of t_output,
  "tt_output type standard table of t_output with empty key.
  "
  "It's important names are short, since CL_SALV_TABLE will flatten them into A-VBELN... etc. - max 30 chars together.
  DATA(table_descr) = cl_abap_tabledescr=>get( cl_abap_structdescr=>get( components )  ).
  FIELD-SYMBOLS: <table> TYPE table.
  DATA table_ref TYPE REF TO data.
  CREATE DATA table_ref TYPE HANDLE table_descr.
  ASSIGN table_ref->* TO <table>.

  "Select data
  SELECT (select) FROM (from) WHERE (where) INTO TABLE @<table>.

  "Display - optionally hide MANDT fields etc.
  NEW zcl_ea_salv_table( )->set_data( REF #( <table> ) )->display_data( ).

AT SELECTION-SCREEN OUTPUT.
  "Hide selections from unused joins
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'J1'. screen-active = COND #( WHEN p_j1 = abap_true THEN 1 ELSE 0 ).
      WHEN 'J2'. screen-active = COND #( WHEN p_j2 = abap_true THEN 1 ELSE 0 ).
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
