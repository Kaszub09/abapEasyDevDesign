"! <p class="shorttext synchronized">Specific use 2</p>
CLASS zcl_edd_bypass_single_use_2 DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_ex_me_gui_po_cust.
ENDCLASS.

CLASS zcl_edd_bypass_single_use_2 IMPLEMENTATION.
  METHOD if_ex_me_gui_po_cust~execute.
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~map_dynpro_fields.
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~subscribe.
    "Some stuff specific to extension 2
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~transport_from_dynp.
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~transport_from_model.
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~transport_to_dynp.
  ENDMETHOD.

  METHOD if_ex_me_gui_po_cust~transport_to_model.
  ENDMETHOD.
ENDCLASS.
