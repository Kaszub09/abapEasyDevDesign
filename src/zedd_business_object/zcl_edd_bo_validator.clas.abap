"! All validations should go there
CLASS zcl_edd_bo_validator DEFINITION PUBLIC CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
     t_char TYPE c LENGTH 1.

    CLASS-METHODS:
      class_constructor,
      validate_new IMPORTING after TYPE zedd_bo EXPORTING errors TYPE zif_edd_bo=>tt_error,
      validate_edit IMPORTING before TYPE zedd_bo after TYPE zedd_bo EXPORTING errors TYPE zif_edd_bo=>tt_error.

    CLASS-DATA:
      zedd_bo_yes_no_values TYPE STANDARD TABLE OF t_char WITH EMPTY KEY READ-ONLY.

  PRIVATE SECTION.
    CLASS-METHODS:
      common_validation IMPORTING after TYPE zedd_bo EXPORTING errors TYPE zif_edd_bo=>tt_error.
ENDCLASS.

CLASS zcl_edd_bo_validator IMPLEMENTATION.
  METHOD class_constructor.
    "Cache data needed for validation
    zedd_bo_yes_no_values = VALUE #( ( abap_false ) ( abap_true ) ).
  ENDMETHOD.

  METHOD validate_new.
    DATA(mat_info) = zcl_edd_bo_cache_mat=>global( )->get_mat_info( after-matnr ).
    IF mat_info->exists = abap_false OR after-matnr IS INITIAL.
      APPEND VALUE #( fields = VALUE #( ( 'MATNR' ) ) error = zcl_ed_msg=>get( text = TEXT-e01 v1 = after-matnr ) ) TO errors.
    ENDIF.

    common_validation( EXPORTING after = after IMPORTING errors = errors ).
  ENDMETHOD.

  METHOD validate_edit.
    common_validation( EXPORTING after = after IMPORTING errors = errors ).
  ENDMETHOD.

  METHOD common_validation.
    "You should check stuff GUI validates anyway, since it could mass uploaded by program etc.
    IF NOT line_exists( zedd_bo_yes_no_values[ table_line = after-is_active ] ).
      APPEND VALUE #( fields = VALUE #( ( 'IS_ACTIVE' ) ) error = zcl_ed_msg=>get( text = TEXT-e02 v1 = after-is_active ) ) TO errors.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
