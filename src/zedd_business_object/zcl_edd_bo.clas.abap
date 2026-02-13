"! NOTE: optimization idea - make bo_current ref, so it doesn't take space until it's actually changed.
"! NOTE: optimization idea - make bo_db ref, so it doesn't take space for new ones.
"!
"!
"! NOTE: optimization idea - cache EXT so it's only recalculated when data was changed
"! NOTE: optimization idea - remember what fields were changed, and then validate/authorize/fill only/change
"! only necessary data based on changed fields
CLASS zcl_edd_bo DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_edd_bo_api.

  PUBLIC SECTION.
    CONSTANTS:
        c_cd_object TYPE cdobjectcl VALUE 'ZEDD_BO'.

    TYPES:
        "! Additional info, texts, descriptions, calculated fields etc.
        t_zedd_bo_ext TYPE zcl_edd_bo_filler=>t_zedd_bo_ext.

    INTERFACES:
      zif_edd_bo.

    ALIASES:
        c_data_type FOR zif_edd_bo~c_data_type,
        lock          FOR zif_edd_bo~lock,
        unlock        FOR zif_edd_bo~unlock,
        is_locked     FOR zif_edd_bo~is_locked,
        validate      FOR zif_edd_bo~validate,
        authorize     FOR zif_edd_bo~authorize,
        save          FOR zif_edd_bo~save,
        reload        FOR zif_edd_bo~reload,
        fill          FOR zif_edd_bo~fill,
        fill_ext          FOR zif_edd_bo~fill_ext,
        change        FOR zif_edd_bo~change,
        change_fields FOR zif_edd_bo~change_fields.

    METHODS:
      get_current_id  IMPORTING data_type TYPE i DEFAULT c_data_type-current RETURNING VALUE(id) TYPE zedd_bo_id RAISING zcx_edd_bo_invalid_data_type.

  PRIVATE SECTION.
    CONSTANTS:
      "Statuses for caching validation/authorization
      BEGIN OF c_status,
        unknown TYPE i VALUE 0,
        ok      TYPE i VALUE 1,
        not_ok  TYPE i VALUE 2,
      END OF c_status.

    METHODS:
      invalidate_statuses,
      save_change_doc.

    METHODS:
      on_save_requested FOR EVENT save_requested OF if_os_transaction.

    DATA:
      is_bo_new            TYPE abap_bool,
      is_bo_locked         TYPE abap_bool,
      validation_status    TYPE i,
      authorization_status TYPE i.

    DATA:
      bo_db      TYPE zedd_bo,
      bo_current TYPE zedd_bo.
ENDCLASS.

CLASS zcl_edd_bo IMPLEMENTATION.
  METHOD lock.
    IF is_bo_locked = abap_true.
      result-is_locked = abap_true.
      RETURN.
    ENDIF.

    IF is_bo_new = abap_true.
      result-is_locked = abap_true.
    ELSE.
      zcl_edd_bo_locker=>lock_record( EXPORTING id = bo_db-bo_id  IMPORTING is_locked = result-is_locked msg = result-msg ).
    ENDIF.

    is_bo_locked = result-is_locked.

    IF result-is_locked = abap_true.
      reload( ). "In case it was changed between read and lock
    ENDIF.
  ENDMETHOD.

  METHOD is_locked.
    is_locked = is_bo_locked.
  ENDMETHOD.

  METHOD unlock.
    IF is_bo_new = abap_true. "new is always locked
      RETURN.
    ENDIF.

    zcl_edd_bo_locker=>unlock_record( bo_db-bo_id ).
    is_bo_locked = abap_false.
  ENDMETHOD.

  METHOD validate.
    IF is_bo_new = abap_true.
      zcl_edd_bo_validator=>validate_new( EXPORTING after = bo_current IMPORTING errors = errors ).
    ELSE.
      zcl_edd_bo_validator=>validate_edit( EXPORTING before = bo_db after = bo_current IMPORTING errors = errors ).
    ENDIF.

    validation_status = COND #( WHEN lines( errors ) = 0 THEN c_status-ok ELSE c_status-not_ok ).
  ENDMETHOD.

  METHOD authorize.
    IF is_bo_new = abap_true.
      zcl_edd_bo_auth=>authorize_new( IMPORTING errors = errors ).
    ELSE.
      zcl_edd_bo_auth=>authorize_edit( EXPORTING before = bo_db after = bo_current IMPORTING errors = errors ).
    ENDIF.

    authorization_status = COND #( WHEN lines( errors ) = 0 THEN c_status-ok ELSE c_status-not_ok ).
  ENDMETHOD.

  METHOD save.
    IF is_bo_locked = abap_false.
      APPEND VALUE #(  error = zcl_ed_msg=>get( text = TEXT-e04 ) ) TO errors.
      RETURN.
    ENDIF.

    IF validation_status = c_status-unknown.
      APPEND LINES OF validate( ) TO errors.
    ENDIF.
    IF authorization_status = c_status-unknown.
      APPEND LINES OF authorize( ) TO errors.
    ENDIF.

    IF authorization_status = c_status-not_ok.
      APPEND VALUE #(  error = zcl_ed_msg=>get( text = TEXT-e02 ) ) TO errors.
    ENDIF.
    IF validation_status = c_status-not_ok.
      APPEND VALUE #(  error = zcl_ed_msg=>get( text = TEXT-e03 ) ) TO errors.
    ENDIF.

    IF lines( errors ) > 0.
      RETURN.
    ENDIF.

    IF is_bo_new = abap_true AND bo_current-bo_id IS INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZEDD_BO'
        IMPORTING
          number                  = bo_current-bo_id
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
        APPEND VALUE #( error = zcl_ed_msg=>get_from_sy( ) ) TO errors.
        RETURN.
      ENDIF.
      zcl_edd_bo_locker=>lock_record( id = bo_current-bo_id ). "Should never fail since it's fresh number
    ENDIF.

    "will be triggered on commit
    SET HANDLER on_save_requested FOR cl_os_system=>get_transaction_manager( )->get_current_transaction( ).
  ENDMETHOD.

  METHOD change.
    invalidate_statuses( ).

    MOVE-CORRESPONDING bo TO bo_current.

    IF is_bo_new = abap_true.
      zcl_edd_bo_filler=>fill_new( CHANGING record = bo_current ).
    ELSE.
      zcl_edd_bo_filler=>fill_changed( EXPORTING before = bo_db CHANGING after = bo_current ).
    ENDIF.
  ENDMETHOD.

  METHOD change_fields.
    invalidate_statuses( ).
    LOOP AT fields REFERENCE INTO DATA(field).
      ASSIGN COMPONENT field->field OF STRUCTURE bo_current TO FIELD-SYMBOL(<field_value>).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_edd_bo_invalid_field EXPORTING field = field->field.
      ENDIF.

      <field_value> = field->value.
    ENDLOOP.

    IF is_bo_new = abap_true.
      zcl_edd_bo_filler=>fill_new( CHANGING record = bo_current ).
    ELSE.
      zcl_edd_bo_filler=>fill_changed( EXPORTING before = bo_db CHANGING after = bo_current ).
    ENDIF.
  ENDMETHOD.

  METHOD fill.
    CASE data_type.
      WHEN c_data_type-db. MOVE-CORRESPONDING bo_db TO bo.
      WHEN c_data_type-current. MOVE-CORRESPONDING bo_current TO bo.
      WHEN OTHERS. RAISE EXCEPTION TYPE zcx_edd_bo_invalid_data_type EXPORTING data_type = data_type.
    ENDCASE.
  ENDMETHOD.

  METHOD reload.
    invalidate_statuses( ).
    IF is_bo_new = abap_false.
      SELECT SINGLE * FROM zedd_bo WHERE bo_id = @bo_db-bo_id INTO @bo_db.
      bo_current = bo_db.
    ENDIF.
  ENDMETHOD.

  METHOD invalidate_statuses.
    validation_status = c_status-unknown.
    authorization_status = c_status-unknown.
  ENDMETHOD.

  METHOD fill_ext.
    CASE data_type.
      WHEN c_data_type-db. zcl_edd_bo_filler=>fill_ext( EXPORTING record = bo_db IMPORTING bo_ext = bo_ext ).
      WHEN c_data_type-current. zcl_edd_bo_filler=>fill_ext( EXPORTING record = bo_current IMPORTING bo_ext = bo_ext ).
      WHEN OTHERS. RAISE EXCEPTION TYPE zcx_edd_bo_invalid_data_type EXPORTING data_type = data_type.
    ENDCASE.
  ENDMETHOD.

  METHOD save_change_doc.
    DATA(cd) = zcl_ed_change_doc_factory=>create( objectclass = c_cd_object table_name = 'ZEDD_BO' ).
    cd->open( objectid = CONV #( bo_current-bo_id ) ).
    IF is_bo_new = abap_true.
      cd->change_single( after = REF #( bo_current ) ).
    ELSE.
      cd->change_single( before = REF #( bo_db ) after = REF #( bo_current ) ).
    ENDIF.
    cd->close( objectid = CONV #( bo_current-bo_id ) ).
  ENDMETHOD.

  METHOD get_current_id.
    CASE data_type.
      WHEN c_data_type-db. id = bo_db-bo_id.
      WHEN c_data_type-current. id = bo_current-bo_id.
      WHEN OTHERS. RAISE EXCEPTION TYPE zcx_edd_bo_invalid_data_type EXPORTING data_type = data_type.
    ENDCASE.
  ENDMETHOD.

  METHOD on_save_requested.
    MODIFY zedd_bo FROM @bo_current.
    save_change_doc( ).

    "Could possibly create BADI called at save,
    "and implementation could take care of e.g. sending emails etc.,
    "but for simplicity sake you can also just edit here.
    is_bo_new = abap_false.
    bo_db = bo_current.
  ENDMETHOD.
ENDCLASS.
