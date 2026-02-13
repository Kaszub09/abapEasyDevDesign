CLASS zcl_edd_bo_api DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      tt_edd_bo TYPE STANDARD TABLE OF REF TO zcl_edd_bo WITH EMPTY KEY,
      "Don't forget to add selections when adding new fields!
      BEGIN OF t_selections,
        bo_id                TYPE RANGE OF zedd_bo-bo_id,
        matnr                TYPE RANGE OF zedd_bo-matnr,
        is_active            TYPE RANGE OF zedd_bo-is_active,
        some_comment         TYPE RANGE OF zedd_bo-some_comment,
        created_at_date      TYPE RANGE OF zedd_bo-created_at_date,
        last_changed_at_date TYPE RANGE OF zedd_bo-last_changed_at_date,
        maktx                TYPE RANGE OF zcl_edd_bo=>t_zedd_bo_ext-maktx,
      END OF t_selections.


    CLASS-METHODS:
      create_bo RETURNING VALUE(bo) TYPE REF TO zcl_edd_bo,
      "! @parameter bo | Not found not bound
      open_bo IMPORTING id TYPE zedd_bo_id RETURNING VALUE(bo) TYPE REF TO zcl_edd_bo,
      "! @parameter load_cache | Loads cache for validation/extensions etc.
      open_bo_by_sel IMPORTING sel        TYPE REF TO t_selections
                               load_cache TYPE abap_bool DEFAULT abap_true
                     RETURNING VALUE(bos) TYPE tt_edd_bo,
      purge_cache.
ENDCLASS.

CLASS zcl_edd_bo_api IMPLEMENTATION.


  METHOD create_bo.
    bo = NEW #( ).
    bo->is_bo_locked = abap_true.
    bo->is_bo_new = bo->is_bo_locked.
    bo->change( VALUE zedd_bo( ) ). "So it's in initial valid state
  ENDMETHOD.


  METHOD open_bo.
    bo = NEW #( ).

    SELECT SINGLE * FROM zedd_bo WHERE bo_id = @id INTO @bo->bo_db.
    IF sy-subrc = 0.
      bo->bo_current = bo->bo_db.
    ELSE.
      FREE: bo. "BO not found
    ENDIF.
  ENDMETHOD.


  METHOD open_bo_by_sel.
    DATA zedd_bo_tab TYPE STANDARD TABLE OF zedd_bo WITH EMPTY KEY.

    "Don't forget to add selections when adding new fields!
    SELECT * FROM zedd_bo_ext
      WHERE bo_id IN @sel->bo_id AND matnr IN @sel->matnr AND is_active IN @sel->is_active AND some_comment IN @sel->some_comment
       AND created_at_date IN @sel->created_at_date AND last_changed_at_date IN @sel->last_changed_at_date AND maktx IN @sel->maktx
      ORDER BY bo_id ASCENDING
      INTO CORRESPONDING FIELDS OF TABLE @zedd_bo_tab.

    IF load_cache = abap_true AND lines( zedd_bo_tab ) > 0.
      zcl_edd_bo_cache_mat=>global( )->load_data( data_table = zedd_bo_tab ).
    ENDIF.

    LOOP AT zedd_bo_tab REFERENCE INTO DATA(zedd_bo).
      DATA(bo) = NEW zcl_edd_bo( ).
      bo->bo_db = zedd_bo->*.
      bo->bo_current = bo->bo_db.
      APPEND bo TO bos.
    ENDLOOP.
  ENDMETHOD.


  METHOD purge_cache.
    zcl_edd_bo_cache_mat=>global( )->clear( ).
  ENDMETHOD.
ENDCLASS.
