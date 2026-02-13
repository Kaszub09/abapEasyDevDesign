"! Buffer example.
"! If possible, use joins.
"! If not, prefer to preload data upfront.
"! Otherwise, just cache data per entry.
"! Buffer only what's needed.
CLASS zcl_edd_buffer_example DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_mat_info,
        matnr TYPE matnr,
        ernam TYPE ernam,
        maktx TYPE maktx,
      END OF t_mat_info,
      tt_mat_info TYPE HASHED TABLE OF t_mat_info WITH UNIQUE KEY matnr.

    METHODS:
      get_mat_info IMPORTING matnr TYPE matnr RETURNING VALUE(mat_info) TYPE REF TO t_mat_info,
      load_data IMPORTING data_table TYPE table matnr_column TYPE fieldname DEFAULT 'MATNR',
      clear.

    CLASS-METHODS:
      "! Global instance to share across programs, or for convenience
      global RETURNING VALUE(global_buffer) TYPE REF TO zcl_edd_buffer_example.

  PRIVATE SECTION.
    CLASS-DATA:
      instance TYPE REF TO zcl_edd_buffer_example.

    DATA:
      buffer TYPE tt_mat_info.
ENDCLASS.

CLASS zcl_edd_buffer_example IMPLEMENTATION.
  METHOD global.
    IF instance IS NOT BOUND.
      instance = NEW #( ).
    ENDIF.

    global_buffer = instance.
  ENDMETHOD.

  METHOD clear.
    CLEAR buffer.
  ENDMETHOD.

  METHOD get_mat_info.
    DATA(material) = REF #( buffer[ matnr = matnr ] OPTIONAL ).
    IF material IS NOT BOUND.
      material = NEW #( matnr = matnr ). "Even if not found in DB, it will be cached

      SELECT SINGLE makt~*, mara~* FROM mara
        LEFT OUTER JOIN makt ON makt~spras = @sy-langu AND makt~matnr = mara~matnr
       WHERE mara~matnr = @matnr
       INTO CORRESPONDING FIELDS OF @material->*.

      INSERT material->* INTO TABLE buffer.
    ENDIF.

    mat_info = material.
  ENDMETHOD.

  METHOD load_data.
    DATA(where) = |mara~matnr = @data_table-{ matnr_column }|.

    SELECT makt~*, mara~* FROM mara
      LEFT OUTER JOIN makt ON makt~spras = @sy-langu AND makt~matnr = mara~matnr
     FOR ALL ENTRIES IN @data_table
     WHERE (where)
     INTO CORRESPONDING FIELDS OF TABLE @buffer.
  ENDMETHOD.

ENDCLASS.
