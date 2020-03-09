*&---------------------------------------------------------------------*
*& Report YJRS_ABAP_0003
*&---------------------------------------------------------------------*
* This programs will measure:
*   - Performance of READ TABLE BINARY SEARCH
*   - Performance of LOOP WHERE with SORTED TABLE
*   - Performance of LOOP with HASHED TABLE
*   - Performance of LOOP WHERE with SECONDARY INDEX SORTED
*   - Performance of LOOP with SECONDARY INDEX HASHED
*   - Performance of PARALLEL CURSOR With READ
*   - Performance of PARALLEL CURSOR Without READ
REPORT YJRS_ABAP_0003.

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
  TYPES: BEGIN OF lt_head,
           docnr TYPE n LENGTH 10,
           gjahr TYPE gjahr,
         END OF lt_head,

         BEGIN OF lt_position,
           docnr TYPE n LENGTH 10,
           gjahr TYPE gjahr,
           posnr TYPE n LENGTH 3,
         END OF lt_position.

  DATA: lo_runtime_1 TYPE REF TO yjrs_runtime_measurement,
        lo_runtime_2 TYPE REF TO yjrs_runtime_measurement,

        lo_random_gjahr TYPE REF TO cl_abap_random_int,
        lo_random_pos TYPE REF TO cl_abap_random_int,

        l_tabix TYPE i,

        l_times_pos TYPE i,

        l_head_lines TYPE i,
        l_pos_lines TYPE i,

        l_total_loops_1 TYPE i,
        l_total_loops_2 TYPE i,
        l_total_loops_3 TYPE i,
        l_total_loops_4 TYPE i,
        l_total_loops_5 TYPE i,
        l_total_loops_6 TYPE i,
        l_total_loops_7 TYPE i,

        li_head TYPE STANDARD TABLE OF lt_head
                WITH HEADER LINE,
        li_head_hashed TYPE HASHED TABLE OF lt_head
                       WITH UNIQUE KEY docnr gjahr
                       WITH HEADER LINE,
        li_head_sec_hashed TYPE STANDARD TABLE OF lt_head
                           WITH DEFAULT KEY
                           WITH UNIQUE HASHED KEY hash_key COMPONENTS docnr gjahr
                           WITH HEADER LINE,
        li_positions TYPE STANDARD TABLE OF lt_position
                     WITH HEADER LINE,
        li_positions_sorted TYPE SORTED TABLE OF lt_position
                            WITH UNIQUE KEY docnr gjahr posnr
                            WITH HEADER LINE,
        li_positions_sec_sorted TYPE STANDARD TABLE OF lt_position
                                WITH DEFAULT KEY
                                WITH UNIQUE SORTED KEY sort_key COMPONENTS docnr gjahr posnr
                                WITH HEADER LINE.

  CREATE OBJECT lo_runtime_1.
  CREATE OBJECT lo_runtime_2.

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
    li_head-docnr = sy-index.
    li_head-gjahr = lo_random_gjahr->get_next( ).
    APPEND li_head.

    INSERT li_head INTO TABLE li_head_hashed.
    INSERT li_head INTO TABLE li_head_sec_hashed.

    IF p_maxpos IS INITIAL.
      l_times_pos = p_minpos.
    ELSE.
      l_times_pos = lo_random_pos->get_next( ).
    ENDIF.

    DO l_times_pos TIMES.
      li_positions-posnr = sy-index.
      li_positions-docnr = li_head-docnr.
      li_positions-gjahr = li_head-gjahr.
      APPEND li_positions.

      INSERT li_positions INTO TABLE li_positions_sorted.
      INSERT li_positions INTO TABLE li_positions_sec_sorted.
    ENDDO.
  ENDDO.
**********************************************************************

  WRITE: / 'Seed:', p_seed.

  WRITE: /.

  WRITE: / 'Head entries:', p_times.
  IF p_maxpos IS INITIAL.
    WRITE: / 'Positions per head:', p_minpos.
  ELSE.
    WRITE: / 'Positions per head:', p_minpos, '-', p_maxpos.
  ENDIF.

  WRITE: /.

  START_MEASUREMENT lo_runtime_1 'STANDARD TABLES - SORT TIME'.
  SORT li_head BY docnr gjahr.
  SORT li_positions BY docnr gjahr posnr.
  END_MEASUREMENT lo_runtime_1 'STANDARD TABLES - SORT TIME'.

  DO p_exec TIMES.
    CLEAR l_total_loops_1.
    START_MEASUREMENT lo_runtime_2 'BINARY SEARCH'.
    LOOP AT li_positions.
      READ TABLE li_head WITH KEY docnr = li_positions-docnr
                                  gjahr = li_positions-gjahr
                         BINARY SEARCH.
      IF sy-subrc = 0.
        ADD 1 TO l_total_loops_1.
      ENDIF.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_2 'BINARY SEARCH'.

    CLEAR l_total_loops_2.
    START_MEASUREMENT lo_runtime_2 'LOOP WHERE SORTED'.
    LOOP AT li_head.
      LOOP AT li_positions_sorted WHERE docnr = li_head-docnr
                                    AND gjahr = li_head-gjahr.
        ADD 1 TO l_total_loops_2.
      ENDLOOP.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_2 'LOOP WHERE SORTED'.

    CLEAR l_total_loops_3.
    START_MEASUREMENT lo_runtime_2 'LOOP HASHED'.
    LOOP AT li_positions.
      READ TABLE li_head_hashed WITH KEY docnr = li_positions-docnr
                                         gjahr = li_positions-gjahr.

      IF sy-subrc = 0.
        ADD 1 TO l_total_loops_3.
      ENDIF.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_2 'LOOP HASHED'.

    CLEAR l_total_loops_4.
    START_MEASUREMENT lo_runtime_2 'LOOP WHERE SECONDARY SORTED'.
    LOOP AT li_head.
      LOOP AT li_positions_sec_sorted INTO li_positions_sec_sorted
                                      USING KEY sort_key
                                      WHERE docnr = li_head-docnr
                                        AND gjahr = li_head-gjahr.
        ADD 1 TO l_total_loops_4.
      ENDLOOP.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_2 'LOOP WHERE SECONDARY SORTED'.

    CLEAR l_total_loops_5.
    START_MEASUREMENT lo_runtime_2 'LOOP SECONDARY HASHED'.
    LOOP AT li_positions.
      READ TABLE li_head_sec_hashed INTO li_head_sec_hashed
                                    WITH TABLE KEY hash_key
                                    COMPONENTS docnr = li_positions-docnr
                                               gjahr = li_positions-gjahr.

      IF sy-subrc = 0.
        ADD 1 TO l_total_loops_5.
      ENDIF.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_2 'LOOP SECONDARY HASHED'.

    CLEAR l_total_loops_6.
    START_MEASUREMENT lo_runtime_2 'PARALLEL CURSOR WITH READ'.
    LOOP AT li_head.
      READ TABLE li_positions WITH KEY docnr = li_head-docnr
                                       gjahr = li_head-gjahr
                              BINARY SEARCH.

      IF sy-subrc = 0.
        l_tabix = sy-tabix.

        LOOP AT li_positions FROM l_tabix.
          IF li_positions-docnr <> li_head-docnr OR
             li_positions-gjahr <> li_head-gjahr.
            EXIT.
          ENDIF.

          ADD 1 TO l_total_loops_6.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_2 'PARALLEL CURSOR WITH READ'.

    CLEAR l_total_loops_7.
    START_MEASUREMENT lo_runtime_2 'PARALLEL CURSOR WITHOUT READ'.
    l_tabix = 1.
    LOOP AT li_head.
      LOOP AT li_positions FROM l_tabix.
        IF li_positions-docnr <> li_head-docnr OR
           li_positions-gjahr <> li_head-gjahr.
          l_tabix = sy-tabix.
          EXIT.
        ENDIF.

        ADD 1 TO l_total_loops_7.
      ENDLOOP.
    ENDLOOP.
    END_MEASUREMENT lo_runtime_2 'PARALLEL CURSOR WITHOUT READ'.
  ENDDO.

  lo_runtime_1->display_measurement( ).

  WRITE: /.

  WRITE: / 'BINARY SEARCH loops:', l_total_loops_1.
  WRITE: / 'LOOP WHERE SORTED loops:', l_total_loops_2.
  WRITE: / 'LOOP HASHED loops:', l_total_loops_3.
  WRITE: / 'LOOP WHERE SECONDARY SORTED loops:', l_total_loops_4.
  WRITE: / 'LOOP SECONDARY HASHED loops:', l_total_loops_5.
  WRITE: / 'PARALLEL CURSOR WITH READ loops:', l_total_loops_6.
  WRITE: / 'PARALLEL CURSOR WITHOUT READ loops:', l_total_loops_7.

  WRITE: /.

  CALL METHOD lo_runtime_2->display_measurement
    EXPORTING
      pi_display_mode = yjrs_runtime_measurement=>e_display_mode-graphical
      pi_time_mode = yjrs_runtime_measurement=>e_time_mode-average.

ENDFORM.
