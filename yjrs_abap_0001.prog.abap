*&---------------------------------------------------------------------*
*& Report YJRS_ABAP_0001
*&---------------------------------------------------------------------*
* This programs will measure:
*   - Performance of LOOP WHERE with STANDARD TABLE (Sorted with SORT)
*   - Performance of LOOP WHERE with SORTED TABLE
*   - Performance using READ TABLE BINARY SEARCH instead of LOOP WHERE with STANDARD TABLE (Sorted with SORT)
*   - Performance using READ TABLE BINARY SEARCH instead of LOOP WHERE with SORTED TABLE
REPORT YJRS_ABAP_0001.

DEFINE START_MEASUREMENT.
  CALL METHOD yjrs_runtime_measurement=>start_measurement
    EXPORTING
      pi_id = &1.
END-OF-DEFINITION.
DEFINE END_MEASUREMENT.
  CALL METHOD yjrs_runtime_measurement=>end_measurement
    EXPORTING
      pi_id = &1.
END-OF-DEFINITION.

START-OF-SELECTION.
  PERFORM start_of_selection.
*&---------------------------------------------------------------------*
*& Form START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM start_of_selection .
  TYPES: BEGIN OF lt_head,
           docnr TYPE n LENGTH 10,
           gjahr TYPE gjahr,
         END OF lt_head,

         BEGIN OF lt_position,
           docnr TYPE n LENGTH 10,
           gjahr TYPE gjahr,
           posnr TYPE n LENGTH 3,
         END OF lt_position.

  DATA: li_head TYPE STANDARD TABLE OF lt_head
                WITH HEADER LINE,
        li_positions TYPE STANDARD TABLE OF lt_position
                     WITH HEADER LINE,
        li_positions_sorted TYPE SORTED TABLE OF lt_position
                            WITH NON-UNIQUE KEY docnr gjahr posnr
                            WITH HEADER LINE.

**********************************************************************
* Populating test internal tables.
  DO 5000 TIMES.
    li_head-docnr = 5001 - sy-index.
    li_head-gjahr = sy-datum(4).
    INSERT TABLE li_head.

    DO 5 TIMES.
      li_positions-posnr = sy-index.
      li_positions-docnr = li_head-docnr.
      li_positions-gjahr = li_head-gjahr.
      INSERT TABLE li_positions.

      li_positions_sorted-posnr = li_positions-posnr.
      li_positions_sorted-docnr = li_head-docnr.
      li_positions_sorted-gjahr = li_head-gjahr.
      INSERT TABLE li_positions_sorted.
    ENDDO.
  ENDDO.
**********************************************************************

  START_MEASUREMENT 'SORT'.
  SORT li_positions BY docnr gjahr posnr.
  END_MEASUREMENT 'SORT'.

  START_MEASUREMENT 'LOOP WHERE'.
  LOOP AT li_head.
    LOOP AT li_positions WHERE docnr = li_head-docnr
                           AND gjahr = li_head-gjahr.
    ENDLOOP.
  ENDLOOP.
  END_MEASUREMENT 'LOOP WHERE'.

  START_MEASUREMENT 'LOOP WHERE SORTED TABLE'.
  LOOP AT li_head.
    LOOP AT li_positions_sorted WHERE docnr = li_head-docnr
                                  AND gjahr = li_head-gjahr.
    ENDLOOP.
  ENDLOOP.
  END_MEASUREMENT 'LOOP WHERE SORTED TABLE'.

  START_MEASUREMENT 'BINARY SEARCH'.
  LOOP AT li_head.
    READ TABLE li_positions WITH KEY docnr = li_head-docnr
                                     gjahr = li_head-gjahr
                            BINARY SEARCH.

    IF sy-subrc = 0.
      LOOP AT li_positions FROM sy-tabix.
        IF li_positions-docnr <> li_head-docnr OR
           li_positions-gjahr <> li_head-gjahr.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  END_MEASUREMENT 'BINARY SEARCH'.

  START_MEASUREMENT 'BINARY SEARCH SORTED TABLE'.
  LOOP AT li_head.
    READ TABLE li_positions_sorted WITH KEY docnr = li_head-docnr
                                            gjahr = li_head-gjahr
                                   BINARY SEARCH.

    IF sy-subrc = 0.
      LOOP AT li_positions_sorted FROM sy-tabix.
        IF li_positions_sorted-docnr <> li_head-docnr OR
           li_positions_sorted-gjahr <> li_head-gjahr.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  END_MEASUREMENT 'BINARY SEARCH SORTED TABLE'.

  yjrs_runtime_measurement=>display_measurement( ).
ENDFORM.
