PROCESS BEFORE OUTPUT.
  CALL SUBSCREEN screen_2_sub INCLUDING sy-repid
screen_2_subscreen.
  MODULE pbo.


PROCESS AFTER INPUT.
  CALL SUBSCREEN screen_2_sub.
  MODULE pai.
