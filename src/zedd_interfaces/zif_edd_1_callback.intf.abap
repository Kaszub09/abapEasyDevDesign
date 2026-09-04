INTERFACE zif_edd_1_callback PUBLIC.
  METHODS:
    "! E.g. getting y may be expensive, so we want to supply it only on demand, not always.
    get_y RETURNING VALUE(y) TYPE decfloat34,
    "! E.g. want user to be able to decide when to refresh display
    "! - instead of refreshing it after every method call in ZIF_EDD_1
    refresh_display.
ENDINTERFACE.
