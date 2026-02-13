"! Simple wrapper for locking/unlocking records.
CLASS zcl_edd_bo_locker DEFINITION PUBLIC FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      lock_record IMPORTING id TYPE zedd_bo_id collect TYPE ddenqcoll DEFAULT space
                  EXPORTING is_locked TYPE abap_bool msg TYPE string,
      lock_collected EXPORTING is_locked TYPE abap_bool msg TYPE string,
      unlock_record IMPORTING id TYPE zedd_bo_id.

  PRIVATE SECTION.
    CONSTANTS:
      c_mode  TYPE  enqmode VALUE 'E',
      c_scope TYPE c LENGTH 1 VALUE '1'.
ENDCLASS.

CLASS zcl_edd_bo_locker IMPLEMENTATION.
  METHOD lock_collected.
    CALL FUNCTION 'FLUSH_ENQUEUE'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      is_locked = abap_true.
    ELSE.
      is_locked = abap_false.
      msg = zcl_ed_msg=>get_from_sy( ).
    ENDIF.
  ENDMETHOD.

  METHOD lock_record.
    CALL FUNCTION 'ENQUEUE_EZEDD_BO'
      EXPORTING
        mode_zedd_bo   = c_mode
        bo_id          = id
        _scope         = c_scope
        _collect       = collect
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      is_locked = abap_true.
    ELSE.
      is_locked = abap_false.
      msg = zcl_ed_msg=>get_from_sy( ).
    ENDIF.
  ENDMETHOD.

  METHOD unlock_record.
    CALL FUNCTION 'DEQUEUE_EZEDD_BO'
      EXPORTING
        mode_zedd_bo = c_mode
        bo_id        = id
        _scope       = c_scope.
  ENDMETHOD.
ENDCLASS.
