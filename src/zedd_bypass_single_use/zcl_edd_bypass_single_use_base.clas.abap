"! <p class="shorttext synchronized">Base of single use</p>
"! THE PROBLEM:
"! You have single use BADI (e.g. IF_EX_ME_GUI_PO_CUST), but you want to extend it multiple times.
"!
"! THE SOLUTION:
"! Create single wrapper that accepts multiple BADI implementations and calls them all.
"!
"! This way you can have multiple extensions, each implementing it's own piece of functionality, that are completely separate.
"! It's also much easier to reason and debug then e.g. slapping includes inside each method.
"!
"! One alternative would be to store references to extension classes (ZCL_EDD_BYPASS_SINGLE_USE_1 etc.) directly,
"! then call them one by one. Or just instantiating them directly inside constructor.
"! But with config table you can switch them on/off with ease, and multiple developers can work each on it's own extension,
"! without any conflicts with transport. It's also easy to deactivate extensions.
"!
"! You can also think of following pattern as BADI that is defined entirely in code instead of SE18,
"! where implementation means adding class with interface to table instead of creating it in SE19.
"! (zcl_edd_bypass_single_use_base - BADI definition, if_ex_me_gui_po_cust - interface, ZCL_EDD_BYPASS_SINGLE_USE_1 - BADI implementation)
CLASS zcl_edd_bypass_single_use_base DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_ex_me_gui_po_cust.

    METHODS:
      constructor.

  PRIVATE SECTION.
    DATA: badis TYPE STANDARD TABLE OF REF TO if_ex_me_gui_po_cust WITH EMPTY KEY.
ENDCLASS.

CLASS zcl_edd_bypass_single_use_base IMPLEMENTATION.
  METHOD constructor.
    SELECT * FROM zedd_cls_config
    WHERE active = @abap_true
    ORDER BY sequence DESCENDING
    INTO TABLE @DATA(classes).

    DATA: badi TYPE REF TO if_ex_me_gui_po_cust.
    LOOP AT classes REFERENCE INTO DATA(class).
      CLEAR badi.
      TRY.
          CREATE OBJECT badi TYPE (class->class).
          APPEND badi TO badis.
        CATCH cx_root. "Better to just continue without this class then straight up crash ME21N. You will notice shortly anyway :)
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~execute.
    "Best case scenario - method has only importing/changing parameters.
    "This way it's easy to just loop through implementations and call them one by one
    LOOP AT badis INTO DATA(badi).
      badi->execute( im_name = im_name im_model = im_model im_fcode = im_fcode ).
    ENDLOOP.
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~map_dynpro_fields.
    LOOP AT badis INTO DATA(badi).
      badi->map_dynpro_fields( CHANGING ch_mapping = ch_mapping ).
    ENDLOOP.
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~subscribe.
    "When methods has returning/exporting parameter, special care must be taken - consider each method individually.
    "Also, when designing interfaces that could potentially be chained, prefer changing to returning - based on this experience.

    "In this case, since returning parameter is table, we combine all returning tables
    LOOP AT badis INTO DATA(badi).
      INSERT LINES OF badi->subscribe( im_application = im_application im_element = im_element ) INTO TABLE re_subscribers.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~transport_from_dynp.
    "There returning parameter is tru if something was changed, and false if not.
    "So we returng true if ant of the underlying BADIs returned true (we OR every result), otherwise return false.
    "Remember about short-circuit evaluation if you want slap method inside xsdbool()
    LOOP AT badis INTO DATA(badi).
      DATA(did_badi_change) = badi->transport_from_dynp( im_name = im_name im_fcode = im_fcode ).
      re_changed = xsdbool( re_changed = abap_true OR did_badi_change = abap_true ).
    ENDLOOP.
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~transport_from_model.
    LOOP AT badis INTO DATA(badi).
      badi->transport_from_model( im_name = im_name im_model = im_model ).
    ENDLOOP.
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~transport_to_dynp.
    LOOP AT badis INTO DATA(badi).
      badi->transport_to_dynp( im_name = im_name ).
    ENDLOOP.
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~transport_to_model.
    LOOP AT badis INTO DATA(badi).
      badi->transport_to_model( im_name = im_name im_model = im_model ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
