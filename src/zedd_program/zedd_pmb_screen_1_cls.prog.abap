"! Starting screen.
CLASS lcl_screen_1 DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      lif_screen.

    METHODS:
      constructor IMPORTING model TYPE REF TO lcl_model,
      call_screen. "Special method in case screen should be called with CALL SCREEN

  PRIVATE SECTION.
    CONSTANTS:
        c_screen TYPE sy-dynnr VALUE '0001'.

    DATA:
      model      TYPE REF TO lcl_model,
      top_screen TYPE REF TO lif_screen.
ENDCLASS.

CLASS lcl_screen_1 IMPLEMENTATION.
  METHOD constructor.
    me->model = model.
  ENDMETHOD.

  METHOD call_screen.
    CALL SCREEN c_screen.
  ENDMETHOD.

  METHOD lif_screen~pai.
    new_screen = me.

    IF top_screen IS BOUND.
      top_screen = top_screen->pai( command = command dynnr = dynnr ).
    ENDIF.

    IF dynnr = c_screen.
      CASE command.
        WHEN 'NEW'.
          model->bo = zcl_edd_bo_api=>create_bo( ).
          model->mode = model->c_mode-new.

        WHEN 'CHANGE'.
          model->bo = zcl_edd_bo_api=>open_bo( zedd_program_mvc_bo_screen_1-bo_id ).
          IF model->bo IS NOT BOUND.
            MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
          DATA(lock_result) = model->bo->lock( ).
          IF lock_result-is_locked = abap_false.
            MESSAGE lock_result-msg TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
          model->mode = model->c_mode-change.

        WHEN 'DISPLAY'.
          model->bo = zcl_edd_bo_api=>open_bo( zedd_program_mvc_bo_screen_1-bo_id ).
          IF model->bo IS NOT BOUND.
            MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
          model->mode = model->c_mode-display.

        WHEN 'EXIT'.
          LEAVE PROGRAM.

        WHEN OTHERS.
          RETURN.
      ENDCASE.

      "When new screen is called on top of existing, we must take special care to assign it before calling it,
      "so POB and PAI events are passed correctly. Once the screen returns (by LEAVE TO SCREEN 0), we can remove reference.
      DATA(new_top_screen) = NEW lcl_screen_2( model ).
      top_screen = new_top_screen.
      new_top_screen->call_screen( ).
      FREE top_screen.
    ENDIF.

  ENDMETHOD.

  METHOD lif_screen~pbo.
    IF top_screen IS BOUND.
      top_screen->pbo( dynnr ).
    ENDIF.

    IF dynnr = c_screen.
      SET PF-STATUS 'STATUS_1'.
    ENDIF.
  ENDMETHOD.



ENDCLASS.
