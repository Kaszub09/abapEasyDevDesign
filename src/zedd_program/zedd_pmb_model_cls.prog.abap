"! Manages/store underlying data, as opposed to view data like in TABLES
CLASS lcl_model DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_mode,
        new     TYPE i VALUE 1,
        change    TYPE i VALUE 2,
        display TYPE i VALUE 3,
      END OF c_mode.
    DATA:
      bo                TYPE REF TO zcl_edd_bo,
      mode TYPE i.
ENDCLASS.

CLASS lcl_model IMPLEMENTATION.
ENDCLASS.
