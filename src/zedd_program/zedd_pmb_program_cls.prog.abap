"! Manages progrma, main responsibility is initialization and passing events to current screen.
CLASS lcl_program DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start,
      pai IMPORTING VALUE(command) TYPE sy-ucomm
                    VALUE(dynnr)   TYPE sy-dynnr,
      pbo IMPORTING VALUE(dynnr) TYPE sy-dynnr.

  PRIVATE SECTION.
    DATA:
      model      TYPE REF TO lcl_model,
      "! Screen on top of current (since we ran this as standard report, on top of 1000)
      top_screen TYPE REF TO lif_screen.
ENDCLASS.

CLASS lcl_program IMPLEMENTATION.
  METHOD start.
    "Starting screen.
    "Also - screen should be either called on top of the current one, or instead.
    "Since we run this as standard report, we must use call, and leave program inside it.
    "Calling in popup would work the same way.
    model = NEW #( ).
    DATA(screen) = NEW lcl_screen_1( model ).
    top_screen = NEW lcl_screen_1( model ).
    screen->call_screen( ).
  ENDMETHOD.

  METHOD pai.
    top_screen = top_screen->pai( command = command dynnr = dynnr ).
  ENDMETHOD.

  METHOD pbo.
    top_screen->pbo( dynnr ).
  ENDMETHOD.

ENDCLASS.
