*&---------------------------------------------------------------------*
*& Report YJRS_ABAP_0004
*&---------------------------------------------------------------------*
* This programs will measure:
REPORT YJRS_ABAP_0004.

DEFINE START_MEASUREMENT.
  CALL METHOD &1->start_measurement
    EXPORTING
      pi_id = &2.
END-OF-DEFINITION.
DEFINE END_MEASUREMENT.
  CALL METHOD &1->end_measurement
    EXPORTING
      pi_id = &2.
END-OF-DEFINITION.

PARAMETERS: p_seed TYPE i DEFAULT 1,
            p_times TYPE i DEFAULT 5000.
SELECTION-SCREEN: BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 5(29) d_postxt.
  PARAMETERS: p_minpos TYPE i DEFAULT 5,
              p_maxpos TYPE i.
SELECTION-SCREEN: END OF LINE.
PARAMETERS: p_ops TYPE i DEFAULT 10000.
PARAMETERS: p_exec TYPE i DEFAULT 100.

INITIALIZATION.
  PERFORM initialization.
START-OF-SELECTION.
  PERFORM start_of_selection.
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization .
  d_postxt = 'Positions'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM start_of_selection .
  TYPES: BEGIN OF lt_position,
           docnr TYPE n LENGTH 10,
           gjahr TYPE gjahr,
           posnr TYPE n LENGTH 3,
         END OF lt_position.

  DATA: lo_runtime_1 TYPE REF TO yjrs_runtime_measurement,
        lo_runtime_2 TYPE REF TO yjrs_runtime_measurement,
        lo_runtime_3 TYPE REF TO yjrs_runtime_measurement,

        lo_random_gjahr TYPE REF TO cl_abap_random_int,
        lo_random_pos TYPE REF TO cl_abap_random_int,

        l_times_pos TYPE i,

        l_docnr TYPE i,
        l_gjahr TYPE gjahr,

        li_operations TYPE STANDARD TABLE OF lt_position
                      WITH HEADER LINE,

        li_positions TYPE STANDARD TABLE OF lt_position
                     WITH HEADER LINE,
        li_positions_sorted TYPE SORTED TABLE OF lt_position
                            WITH UNIQUE KEY docnr gjahr posnr
                            WITH HEADER LINE,
        li_positions_hashed TYPE HASHED TABLE OF lt_position
                            WITH UNIQUE KEY docnr gjahr posnr
                            WITH HEADER LINE,
        li_positions_sec_sorted TYPE STANDARD TABLE OF lt_position
                                WITH UNIQUE SORTED KEY sort_key
                                COMPONENTS docnr gjahr posnr
                                WITH HEADER LINE,
        li_positions_sec_hashed TYPE STANDARD TABLE OF lt_position
                                WITH UNIQUE SORTED KEY hash_key
                                COMPONENTS docnr gjahr posnr
                                WITH HEADER LINE.

  CREATE OBJECT lo_runtime_1.
  CREATE OBJECT lo_runtime_2.
  CREATE OBJECT lo_runtime_3.

**********************************************************************
* Populating test internal tables.
  CALL METHOD cl_abap_random_int=>create
    EXPORTING
      seed           = p_seed
      min            = 2000
      max            = 2020
    RECEIVING
      prng           = lo_random_gjahr.

  IF NOT p_maxpos IS INITIAL.
    CALL METHOD cl_abap_random_int=>create
      EXPORTING
        seed           = p_seed
        min            = p_minpos
        max            = p_maxpos
      RECEIVING
        prng           = lo_random_pos.
  ENDIF.

  DO p_times TIMES.
    l_docnr = sy-index.
    l_gjahr = lo_random_gjahr->get_next( ).

    IF p_maxpos IS INITIAL.
      l_times_pos = p_minpos.
    ELSE.
      l_times_pos = lo_random_pos->get_next( ).
    ENDIF.

    DO l_times_pos TIMES.
      li_positions-posnr = sy-index.
      li_positions-docnr = l_docnr.
      li_positions-gjahr = l_gjahr.

      APPEND li_positions.
    ENDDO.
  ENDDO.

  INSERT LINES OF li_positions INTO TABLE li_positions_sorted.
  INSERT LINES OF li_positions INTO TABLE li_positions_hashed.
  INSERT LINES OF li_positions INTO TABLE li_positions_sec_sorted.
  INSERT LINES OF li_positions INTO TABLE li_positions_sec_hashed.
**********************************************************************

  WRITE: / 'Seed:', p_seed.

  WRITE: /.

  WRITE: / 'Head entries:', p_times.
  IF p_maxpos IS INITIAL.
    WRITE: / 'Positions per head:', p_minpos.
  ELSE.
    WRITE: / 'Positions per head:', p_minpos, '-', p_maxpos.
  ENDIF.

  WRITE: / 'Total positions:', LINES( li_positions ).

  WRITE: /.

  DO p_ops TIMES.
    ADD 1 TO l_docnr.
    l_gjahr = lo_random_gjahr->get_next( ).

    li_operations-posnr = sy-index.
    li_operations-docnr = l_docnr.
    li_operations-gjahr = l_gjahr.
    APPEND li_operations.
  ENDDO.

  DO p_exec TIMES.
    START_MEASUREMENT lo_runtime_1 'APPEND STANDARD TABLE'.
    LOOP AT li_operations.
      APPEND li_operations TO li_positions.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_1 'APPEND STANDARD TABLE'.

    START_MEASUREMENT lo_runtime_1 'INSERT SORTED TABLE'.
    LOOP AT li_operations.
      INSERT li_operations INTO TABLE li_positions_sorted.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_1 'INSERT SORTED TABLE'.

    START_MEASUREMENT lo_runtime_1 'INSERT HASHED TABLE'.
    LOOP AT li_operations.
      INSERT li_operations INTO TABLE li_positions_hashed.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_1 'INSERT HASHED TABLE'.

    START_MEASUREMENT lo_runtime_1 'INSERT SEC SORTED TABLE'.
    LOOP AT li_operations.
      INSERT li_operations INTO TABLE li_positions_sec_sorted.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_1 'INSERT SEC SORTED TABLE'.

    START_MEASUREMENT lo_runtime_1 'INSERT SEC HASHED TABLE'.
    LOOP AT li_operations.
      INSERT li_operations INTO TABLE li_positions_sec_hashed.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_1 'INSERT SEC HASHED TABLE'.

    SORT li_positions BY docnr gjahr posnr.

    START_MEASUREMENT lo_runtime_2 'DELETE STANDARD TABLE'.
    LOOP AT li_operations.
      READ TABLE li_positions WITH KEY docnr = li_operations-docnr
                                       gjahr = li_operations-gjahr
                                       posnr = li_operations-posnr
                              BINARY SEARCH.

      IF sy-subrc = 0.
        DELETE li_positions INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_2 'DELETE STANDARD TABLE'.

    START_MEASUREMENT lo_runtime_2 'DELETE SORTED TABLE'.
    LOOP AT li_operations.
      DELETE li_positions_sorted WHERE docnr = li_operations-docnr
                                   AND gjahr = li_operations-gjahr
                                   AND posnr = li_operations-posnr.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_2 'DELETE SORTED TABLE'.

    START_MEASUREMENT lo_runtime_2 'DELETE HASHED TABLE'.
    LOOP AT li_operations.
      DELETE li_positions_hashed WHERE docnr = li_operations-docnr
                                   AND gjahr = li_operations-gjahr
                                   AND posnr = li_operations-posnr.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_2 'DELETE HASHED TABLE'.

    START_MEASUREMENT lo_runtime_2 'DELETE SEC SORTED TABLE'.
    LOOP AT li_operations.
      DELETE li_positions_sec_sorted USING KEY sort_key
                                     WHERE docnr = li_operations-docnr
                                       AND gjahr = li_operations-gjahr
                                       AND posnr = li_operations-posnr.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_2 'DELETE SEC SORTED TABLE'.

    START_MEASUREMENT lo_runtime_2 'DELETE SEC HASHED TABLE'.
    LOOP AT li_operations.
      DELETE li_positions_sec_hashed USING KEY hash_key
                                     WHERE docnr = li_operations-docnr
                                       AND gjahr = li_operations-gjahr
                                       AND posnr = li_operations-posnr.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_2 'DELETE SEC HASHED TABLE'.

    START_MEASUREMENT lo_runtime_3 'APPEND STANDARD TABLE (LINES)'.
    APPEND LINES OF li_operations TO li_positions.
    END_MEASUREMENT lo_runtime_3 'APPEND STANDARD TABLE (LINES)'.

    START_MEASUREMENT lo_runtime_3 'INSERT SORTED TABLE (LINES)'.
    INSERT LINES OF li_operations INTO TABLE li_positions_sorted.
    END_MEASUREMENT lo_runtime_3 'INSERT SORTED TABLE (LINES)'.

    START_MEASUREMENT lo_runtime_3 'INSERT HASHED TABLE (LINES)'.
    INSERT LINES OF li_operations INTO TABLE li_positions_hashed.
    END_MEASUREMENT lo_runtime_3 'INSERT HASHED TABLE (LINES)'.

    START_MEASUREMENT lo_runtime_3 'INSERT SEC SORTED TABLE (LINES)'.
    INSERT LINES OF li_operations INTO TABLE li_positions_sec_sorted.
    END_MEASUREMENT lo_runtime_3 'INSERT SEC SORTED TABLE (LINES)'.

    START_MEASUREMENT lo_runtime_3 'INSERT SEC HASHED TABLE (LINES)'.
    INSERT LINES OF li_operations INTO TABLE li_positions_sec_hashed.
    END_MEASUREMENT lo_runtime_3 'INSERT SEC HASHED TABLE (LINES)'.

**********************************************************************
*   Tables cleaning.
    SORT li_positions BY docnr gjahr posnr.

    LOOP AT li_operations.
      READ TABLE li_positions WITH KEY docnr = li_operations-docnr
                                       gjahr = li_operations-gjahr
                                       posnr = li_operations-posnr
                              BINARY SEARCH.

      IF sy-subrc = 0.
        DELETE li_positions INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    LOOP AT li_operations.
      DELETE li_positions_sorted WHERE docnr = li_operations-docnr
                                   AND gjahr = li_operations-gjahr
                                   AND posnr = li_operations-posnr.
    ENDLOOP.

    LOOP AT li_operations.
      DELETE li_positions_hashed WHERE docnr = li_operations-docnr
                                   AND gjahr = li_operations-gjahr
                                   AND posnr = li_operations-posnr.
    ENDLOOP.

    LOOP AT li_operations.
      DELETE li_positions_sec_sorted USING KEY sort_key
                                     WHERE docnr = li_operations-docnr
                                       AND gjahr = li_operations-gjahr
                                       AND posnr = li_operations-posnr.
    ENDLOOP.

    LOOP AT li_operations.
      DELETE li_positions_sec_hashed USING KEY hash_key
                                     WHERE docnr = li_operations-docnr
                                       AND gjahr = li_operations-gjahr
                                       AND posnr = li_operations-posnr.
    ENDLOOP.
**********************************************************************
  ENDDO.

  WRITE: / '* INSERT performance'.
  ULINE.
  CALL METHOD lo_runtime_1->display_measurement
    EXPORTING
      pi_display_mode = yjrs_runtime_measurement=>e_display_mode-graphical
      pi_time_mode    = yjrs_runtime_measurement=>e_time_mode-average.

  WRITE: /.

  WRITE: / '* INSERT (LINES) performance'.
  ULINE.
  CALL METHOD lo_runtime_3->display_measurement
    EXPORTING
      pi_display_mode = yjrs_runtime_measurement=>e_display_mode-graphical
      pi_time_mode    = yjrs_runtime_measurement=>e_time_mode-average.

  WRITE: /.

  WRITE: / '* DELETE performance'.
  ULINE.
  CALL METHOD lo_runtime_2->display_measurement
    EXPORTING
      pi_display_mode = yjrs_runtime_measurement=>e_display_mode-graphical
      pi_time_mode    = yjrs_runtime_measurement=>e_time_mode-average.
ENDFORM.
