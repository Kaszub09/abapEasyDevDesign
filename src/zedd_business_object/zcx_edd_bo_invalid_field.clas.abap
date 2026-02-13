CLASS zcx_edd_bo_invalid_field DEFINITION PUBLIC INHERITING FROM cx_dynamic_check FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
        field    TYPE fieldname.

    DATA:
        field TYPE fieldname READ-ONLY.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcx_edd_bo_invalid_field IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
        previous = previous ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->field = field.
  ENDMETHOD.
ENDCLASS.
