"! Imagine you create an interface that's supposed to be used to extend your program (like BADI).
"!
"! If possible, you create methods where user may want to take an action or modify some data,
"! and supply all data via parameters.
"!
"! But sometimes it's not enough - e.g. preparing data is costly so you want to supply it only on request,
"! or you want user to trigger some specific action at will.
"! In short, extend communication between program and interface implementer.
"!
"! You can achieve this by creating another interface (like ZIF_EDD_1_CALLBACK), implementing it in program,
"! and then supplying via parameters in ZIF_EDD_1.
INTERFACE zif_edd_1 PUBLIC.
  METHODS:
    "! Supply callback once and let user store the reference (or supply it in every method)
    on_start IMPORTING config TYPE string callback TYPE REF TO zif_edd_1_callback,
    before_display IMPORTING alv TYPE REF TO cl_gui_alv_grid,
    calculate_x IMPORTING base TYPE decfloat34 RETURNING VALUE(result) TYPE decfloat34.

  DATA:
      "! Alternatively, you could assign it to variable.
      callback_alt TYPE REF TO zif_edd_1_callback.
ENDINTERFACE.
