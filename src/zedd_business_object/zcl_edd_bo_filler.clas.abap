"! Fill additional data.
"! E.g. creation date of record, last changed by etc.
"! Also additional text etc.
CLASS zcl_edd_bo_filler DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      "! Additional info, texts, descriptions, calculated fields etc.
      BEGIN OF t_zedd_bo_ext,
        maktx TYPE maktx,
      END OF t_zedd_bo_ext.

    CLASS-METHODS:
      class_constructor,
      fill_new CHANGING record TYPE zedd_bo,
      fill_changed IMPORTING before TYPE zedd_bo CHANGING after TYPE zedd_bo,
      fill_ext IMPORTING record TYPE zedd_bo EXPORTING bo_ext TYPE any.

    CLASS-DATA:
      "! Other fields will be filled automatically
      editable_fields_new    TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line READ-ONLY,
      "! Other fields will be filled automatically or unchanged
      editable_fields_change TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line READ-ONLY.

  PRIVATE SECTION.
    CLASS-DATA:
      non_editable_fields_change TYPE STANDARD TABLE OF fieldname WITH EMPTY KEY.
ENDCLASS.

CLASS zcl_edd_bo_filler IMPLEMENTATION.
  METHOD class_constructor.
    "Field is either: editable / noneditable / filled by filler
    editable_fields_new = VALUE #( ( |MATNR| ) ( |SOME_COMMENT| ) ).
    editable_fields_change = VALUE #( ( |IS_ACTIVE| ) ( |SOME_COMMENT| ) ).
    non_editable_fields_change = VALUE #( ( |BO_ID| ) ( |MATNR| ) ( |CREATED_AT_DATE| ) ).

    "Also cache data that will be needed when filling new/changed
  ENDMETHOD.

  METHOD fill_new.
    "ID Will be filled at save
    "Clear rest of fields that should be empty
    CLEAR: record-bo_id, record-is_active, record-last_changed_at_date.

    "Some fields may be filled based on other, some with default values etc.
    record-created_at_date = sy-datum.
    "If there is overlap with FILL_CHANGED, move common code to another method.
  ENDMETHOD.

  METHOD fill_changed.
    after-last_changed_at_date = sy-datum.

    "Fields that shouldn't be edited
    LOOP AT non_editable_fields_change REFERENCE INTO DATA(non_editable_field).
      ASSIGN COMPONENT non_editable_field->* OF STRUCTURE before TO FIELD-SYMBOL(<non_editable_field_before>).
      ASSIGN COMPONENT non_editable_field->* OF STRUCTURE after TO FIELD-SYMBOL(<non_editable_field_after>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <non_editable_field_before> <> <non_editable_field_after>.
        <non_editable_field_after> = <non_editable_field_before>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_ext.
    DATA ext TYPE t_zedd_bo_ext.

    DATA(mat_info) = zcl_edd_bo_cache_mat=>global( )->get_mat_info( record-matnr ).
    ext-maktx = mat_info->maktx.

    MOVE-CORRESPONDING ext TO bo_ext.
  ENDMETHOD.
ENDCLASS.
