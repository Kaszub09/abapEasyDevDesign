"! Loosely based on MVC, could probably be separated further on - e.g. screen classes
"! could return action to execute etc., but this much abstraction is probably enough
"!
"! We will use business object example from another EDD package
REPORT zedd_program_mvc_bo.

"=================================================================
"Screen elements - required to be globals, so you can separate them cleanly into their own structures
"-----------------------------------------------------------------
TABLES:
  zedd_program_mvc_bo_screen_1,
  zedd_program_mvc_bo_screen_2.

DATA:
 screen_2_subscreen TYPE sy-dynnr.

"=================================================================
"All the classes required
"-----------------------------------------------------------------
"Classes are in order which makes it work for compiler, but you would probably start writing
"them from the bottom of the screen stack. So first program, then screen 1, screen 2 etc., editing model as needed.
"Possible screens stacks:
"1
"1-->2==>3(SUB)
"1-->2==>4(SUB)
"1-->5
"Some common interface for screen class
INTERFACE lif_screen.
  METHODS:
    pai IMPORTING command           TYPE sy-ucomm
                  dynnr             TYPE sy-dynnr
        RETURNING VALUE(new_screen) TYPE REF TO lif_screen,
    pbo IMPORTING dynnr TYPE sy-dynnr.
ENDINTERFACE.

INCLUDE zedd_pmb_model_cls. "Model class which manages data

"Each screen is identified with it's own class, but they could be shared in needed
"(e.g. same class for two screens with different layout but same fields)
INCLUDE zedd_pmb_screen_5_cls. "Some screen to switch to demonstrate SET SCREEN.
INCLUDE zedd_pmb_screen_2_cls. "Main BO screen
INCLUDE zedd_pmb_screen_1_cls. "Starting screen.

INCLUDE zedd_pmb_program_cls. "Top level class that manages application.

"=================================================================
"Program start
"-----------------------------------------------------------------
"This way we can treat it as report on top of selection screen, and exit with leave program

INITIALIZATION.
  DATA(program) = NEW lcl_program( ).
  program->start( ).

  "=================================================================
  "Module - every screen should use same module,
  "command will be passed to correct one through program class.
  "Since each class has reference to child (screen on top of it if it exists),
  "if will be correctly passed down the screen stack
MODULE pbo OUTPUT.
  program->pbo( dynnr = sy-dynnr ).
ENDMODULE.

MODULE pai INPUT.
  program->pai( command = sy-ucomm dynnr = sy-dynnr ).
ENDMODULE.
