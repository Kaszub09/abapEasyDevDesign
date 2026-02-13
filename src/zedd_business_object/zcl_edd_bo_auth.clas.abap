CLASS zcl_edd_bo_auth DEFINITION PUBLIC CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      can_create IMPORTING user TYPE sy-uname DEFAULT sy-uname RETURNING VALUE(can) TYPE abap_bool,
      can_edit IMPORTING user TYPE sy-uname DEFAULT sy-uname RETURNING VALUE(can) TYPE abap_bool,
      can_edit_field IMPORTING user TYPE sy-uname DEFAULT sy-uname field TYPE fieldname RETURNING VALUE(can) TYPE abap_bool,
      authorize_new IMPORTING user TYPE sy-uname DEFAULT sy-uname EXPORTING errors TYPE zif_edd_bo=>tt_error,
      authorize_edit IMPORTING user TYPE sy-uname DEFAULT sy-uname before TYPE zedd_bo after TYPE zedd_bo EXPORTING errors TYPE zif_edd_bo=>tt_error.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_user_field_edit_cache,
        user  TYPE sy-uname,
        field TYPE fieldname,
        can   TYPE abap_bool,
      END OF t_user_field_edit_cache,
      tt_user_field_edit_cache TYPE SORTED TABLE OF t_user_field_edit_cache WITH UNIQUE KEY user field.

    CONSTANTS:
      c_auth_object TYPE c LENGTH 7 VALUE 'ZEDD_BO',
      BEGIN OF c_activity,
        id   TYPE c LENGTH 5 VALUE 'ACTVT',
        new  TYPE c LENGTH 2 VALUE '01',
        edit TYPE c LENGTH 2 VALUE '02',
      END OF c_activity,
      c_fieldname_id     TYPE c LENGTH 30 VALUE 'FIELDNAME',
      c_fieldname_create TYPE c LENGTH 30 VALUE 'DUMMY_FIELDNAME_CAN_CREATE',
      c_fieldname_edit   TYPE c LENGTH 30 VALUE 'DUMMY_FIELDNAME_CAN_EDIT'.

    CLASS-DATA:
      user_field_edit_cache  TYPE tt_user_field_edit_cache.
ENDCLASS.

CLASS zcl_edd_bo_auth IMPLEMENTATION.
  METHOD authorize_edit.
    IF NOT can_edit( user ).
      APPEND VALUE #( error = zcl_ed_msg=>get( text = TEXT-e02 v1 = c_auth_object ) ) TO errors.
    ENDIF.

    LOOP AT zcl_edd_bo_filler=>editable_fields_change REFERENCE INTO DATA(editable_field).
      ASSIGN COMPONENT editable_field->* OF STRUCTURE before TO FIELD-SYMBOL(<editable_field_before>).
      ASSIGN COMPONENT editable_field->* OF STRUCTURE after TO FIELD-SYMBOL(<editable_field_after>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <editable_field_before> <> <editable_field_after> AND ( NOT can_edit_field( user = user field = editable_field->* ) ).
        APPEND VALUE #( fields = VALUE #( ( editable_field->* ) ) error = zcl_ed_msg=>get( text = TEXT-e01 v1 = editable_field->* ) ) TO errors.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD authorize_new.
    IF NOT can_create( user ).
      APPEND VALUE #( error = zcl_ed_msg=>get( text = TEXT-e02 v1 = c_auth_object ) ) TO errors.
    ENDIF.
  ENDMETHOD.

  METHOD can_create.
    DATA(user_field_edit) = REF #( user_field_edit_cache[ user = user field = c_fieldname_create  ] OPTIONAL ).
    IF user_field_edit IS NOT BOUND.
      AUTHORITY-CHECK OBJECT c_auth_object FOR USER user ID c_activity-id FIELD c_activity-new.
      INSERT VALUE #( user = user field = c_fieldname_create can = xsdbool( sy-subrc = 0 ) )
      INTO TABLE user_field_edit_cache REFERENCE INTO user_field_edit.
    ENDIF.

    can = user_field_edit->can.
  ENDMETHOD.

  METHOD can_edit.
    DATA(user_field_edit) = REF #( user_field_edit_cache[ user = user field = c_fieldname_edit ] OPTIONAL ).

    IF user_field_edit IS NOT BOUND.
      AUTHORITY-CHECK OBJECT c_auth_object FOR USER user ID c_activity-id FIELD c_activity-edit.
      INSERT VALUE #( user = user field = c_fieldname_edit can = xsdbool( sy-subrc = 0 ) )
      INTO TABLE user_field_edit_cache REFERENCE INTO user_field_edit.
    ENDIF.

    can = user_field_edit->can.
  ENDMETHOD.

  METHOD can_edit_field.
    DATA(user_field_edit) = REF #( user_field_edit_cache[ user = user field = field ] OPTIONAL ).
    IF user_field_edit IS NOT BOUND.
      AUTHORITY-CHECK OBJECT c_auth_object FOR USER user ID c_fieldname_id FIELD field.
      INSERT VALUE #( user = user field = field can = xsdbool( sy-subrc = 0 ) )
      INTO TABLE user_field_edit_cache REFERENCE INTO user_field_edit.
    ENDIF.
    can = user_field_edit->can.
  ENDMETHOD.
ENDCLASS.
