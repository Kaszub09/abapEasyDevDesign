"! Starting screen.
CLASS lcl_screen_5 DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      lif_screen.

    METHODS:
      constructor IMPORTING model TYPE REF TO lcl_model.
  PRIVATE SECTION.
    CONSTANTS:
        c_screen TYPE sy-dynnr VALUE '0005'.

    "Final screen, so no need for top_screen variable
    DATA:
      model      TYPE REF TO lcl_model .
ENDCLASS.

CLASS lcl_screen_5 IMPLEMENTATION.
  METHOD constructor.
    me->model = model.
    SET SCREEN c_screen.
  ENDMETHOD.

  METHOD lif_screen~pai.
    new_screen = me.

    IF dynnr = c_screen.
      CASE command.
        WHEN 'GO_START'.
          LEAVE TO SCREEN 0.

        WHEN OTHERS.
          RETURN.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD lif_screen~pbo.
    SET PF-STATUS 'STATUS_5'.
  ENDMETHOD.



ENDCLASS.
