"! Generic interface with some types definition common across objects.
"! But you might as well use specific instance for type safety.
"!
"! Access BO via ZCL_EDD_BO_API.
"!
"! How to implement BO:
"! 1. Start with table and elements definitions - along with check tables etc.
"! 2. Create BO class - you can basically copy the example, most of logic will stay the same
"! 2.1 It may be possible to extract it into generic BO with interface, which each specific BO type would implement,
"! but that would probably be on overkill and too many abstractions just to avoid small repetition
"! 3. Start creating and implementing classes
"! 3.1 You might go from top, so: filler, locker, validation, authorization, change doc, numerator, API.
"! Cache as you go.
"!
"! You might think about some mass change API, if there will be need.
INTERFACE zif_edd_bo PUBLIC.
  TYPES:
    BEGIN OF t_lock_result,
      is_locked TYPE abap_bool,
      msg       TYPE string,
    END OF t_lock_result.

  TYPES:
    BEGIN OF t_error,
      fields TYPE STANDARD TABLE OF fieldname WITH EMPTY KEY,
      error  TYPE string,
    END OF t_error,
    tt_error TYPE STANDARD TABLE OF t_error WITH EMPTY KEY,
    BEGIN OF t_fields,
      field TYPE fieldname,
      value TYPE string,
    END OF t_fields,
    tt_fields TYPE STANDARD TABLE OF t_fields WITH EMPTY KEY.

  CONSTANTS:
    BEGIN OF c_data_type,
      db      TYPE i VALUE 1,
      current TYPE i VALUE 2,
    END OF c_data_type.

  METHODS:
    lock RETURNING VALUE(result) TYPE t_lock_result,
    unlock,
    is_locked RETURNING VALUE(is_locked) TYPE abap_bool.

  METHODS:
    "! Logical data validations.
    validate RETURNING VALUE(errors) TYPE tt_error,
    "! Change authorizations.
    authorize RETURNING VALUE(errors) TYPE tt_error,
    "! You must use explicit COMMIT WORK AND WAIT for changes to be reflected in database.
    "! <br/>Locks are not released, and new objects are locked.
    save RETURNING VALUE(errors) TYPE tt_error.

  METHODS:
    "! Reload data from db, called also when existing object is locked. Overwrites current data for existing objects.
    "! <br/>Lock is not released.
    reload,
    fill IMPORTING data_type TYPE i DEFAULT c_data_type-current EXPORTING bo TYPE any RAISING zcx_edd_bo_invalid_data_type,
    "! Fills additional data, e.g. material texts, transient data etc.
    fill_ext IMPORTING data_type TYPE i DEFAULT c_data_type-current EXPORTING bo_ext TYPE any RAISING zcx_edd_bo_invalid_data_type,
    change IMPORTING bo TYPE any,
    change_fields IMPORTING fields TYPE tt_fields RETURNING VALUE(errors) TYPE tt_error RAISING zcx_edd_bo_invalid_field.
ENDINTERFACE.
