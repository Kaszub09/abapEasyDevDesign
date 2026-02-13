"! BO main screen
CLASS lcl_screen_2 DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      lif_screen.

    METHODS:
      constructor IMPORTING model TYPE REF TO lcl_model,
      call_screen.
  PRIVATE SECTION.
    CONSTANTS:
        c_screen TYPE sy-dynnr VALUE '0002'.

    DATA:
      model      TYPE REF TO lcl_model,
      top_screen TYPE REF TO lif_screen.
ENDCLASS.

CLASS lcl_screen_2 IMPLEMENTATION.
  METHOD constructor.
    me->model = model.
    screen_2_subscreen = '0003'.
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
      IF model->mode <> model->c_mode-display.
        model->bo->change( zedd_program_mvc_bo_screen_2 ).
      ENDIF.

      CASE command.
        WHEN 'BACK'.
          model->bo->unlock( ).
          LEAVE TO SCREEN 0.

        WHEN 'MSG_I'.
          MESSAGE TEXT-002 TYPE 'I'.

          "You could, if needed create own classes for subscreens,
          "store them in table, and pass PAI to them.
          "Excercise for the reader :)
        WHEN 'SUB1'.
          screen_2_subscreen = '0003'.

        WHEN 'SUB2'.
          screen_2_subscreen = '0004'.

        WHEN 'SAVE'.
          DATA(errors) = model->bo->save( ).
          IF lines( errors ) = 0.
            COMMIT WORK AND WAIT.
            MESSAGE TEXT-003 TYPE 'I'.
          ELSE.
            LOOP AT errors REFERENCE INTO DATA(err).
              MESSAGE err->error TYPE 'I'.
            ENDLOOP.
          ENDIF.

        WHEN 'DEAD_END'.
          "This screen is instantiated using SET SCREEN, so it replaces current screen.
          "In such case we just return new screen.
          "Do some cleaning if necessary.
          model->bo->unlock( ).
          new_screen = NEW lcl_screen_5( model ).

        WHEN OTHERS.
          RETURN.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD lif_screen~pbo.
    IF top_screen IS BOUND.
      top_screen->pbo( dynnr ).
    ENDIF.

    IF dynnr = c_screen.
      IF model->mode = model->c_mode-display.
        SET PF-STATUS 'STATUS_2' EXCLUDING 'SAVE'.
      ELSE.
        SET PF-STATUS 'STATUS_2'.
      ENDIF.
      model->bo->fill( IMPORTING bo = zedd_program_mvc_bo_screen_2 ).
      model->bo->fill_ext( IMPORTING bo_ext = zedd_program_mvc_bo_screen_2 ).

      LOOP AT SCREEN.
        IF screen-group1 = 'BOF'.
          SPLIT screen-name AT '-' INTO DATA(dummy) DATA(field).
          IF ( model->mode = model->c_mode-display )
          OR ( model->mode = model->c_mode-change AND NOT line_exists( zcl_edd_bo_filler=>editable_fields_change[ table_line = field ] ) )
          OR ( model->mode = model->c_mode-new AND NOT line_exists( zcl_edd_bo_filler=>editable_fields_new[ table_line = field ] ) ).
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
