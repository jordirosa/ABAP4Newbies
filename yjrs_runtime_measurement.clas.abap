class YJRS_RUNTIME_MEASUREMENT definition
  public
  final
  create public .

public section.

  types:
    T_CLOCK_RESOLUTION TYPE C LENGTH 1 .
  types:
    T_DISPLAY_MODE TYPE C LENGTH 1 .
  types:
    T_TIME_MODE TYPE C LENGTH 1 .
  types:
    T_MEASUREMENT_ID TYPE c LENGTH 50 .

  constants:
    BEGIN OF E_CLOCK_RESOLUTION,
               LOW TYPE T_CLOCK_RESOLUTION VALUE '1',
               HIGH TYPE T_CLOCK_RESOLUTION VALUE '2',
             END OF E_CLOCK_RESOLUTION .
  constants:
    BEGIN OF E_DISPLAY_MODE,
               TEXT TYPE T_DISPLAY_MODE VALUE '1',
               GRAPHICAL TYPE T_DISPLAY_MODE VALUE '2',
             END OF E_DISPLAY_MODE .
  constants:
    BEGIN OF E_TIME_MODE,
               SUM TYPE T_TIME_MODE VALUE '1',
               AVERAGE TYPE T_TIME_MODE VALUE '2',
             END OF E_TIME_MODE .

  class-methods SET_CLOCK_RESOLUTION
    importing
      !PI_CLOCK_RESOLUTION type T_CLOCK_RESOLUTION .
  methods CLEAR_MEASUREMENT .
  methods START_MEASUREMENT
    importing
      !PI_ID type T_MEASUREMENT_ID .
  methods END_MEASUREMENT
    importing
      !PI_ID type T_MEASUREMENT_ID .
  methods DISPLAY_MEASUREMENT
    importing
      !PI_DISPLAY_MODE type T_DISPLAY_MODE default E_DISPLAY_MODE-TEXT
      !PI_TIME_MODE type T_TIME_MODE default E_TIME_MODE-SUM .
protected section.
private section.

  types:
    BEGIN OF T_MEASUREMENT,
           id TYPE t_measurement_id,
           start_time TYPE i,
           end_time TYPE i,
           total_time TYPE i,
           total_executions TYPE i,
           position TYPE i,
         END OF T_MEASUREMENT .
  types:
    t_measurement_tab TYPE SORTED TABLE OF t_measurement WITH UNIQUE KEY id .

  data MEASUREMENT_TAB type T_MEASUREMENT_TAB .

  methods HELPER_DISPLAY_TEXT
    importing
      !PI_TIME_MODE type T_TIME_MODE default E_TIME_MODE-SUM .
  methods HELPER_DISPLAY_GRAPHICAL
    importing
      !PI_TIME_MODE type T_TIME_MODE default E_TIME_MODE-SUM .
ENDCLASS.



CLASS YJRS_RUNTIME_MEASUREMENT IMPLEMENTATION.


  method CLEAR_MEASUREMENT.
    REFRESH: measurement_tab.
  endmethod.


  method DISPLAY_MEASUREMENT.
    CASE pi_display_mode.
      WHEN e_display_mode-text.
        CALL METHOD helper_display_text
          EXPORTING
            pi_time_mode = pi_time_mode.
      WHEN e_display_mode-graphical.
        CALL METHOD helper_display_graphical
          EXPORTING
            pi_time_mode = pi_time_mode.
    ENDCASE.
  endmethod.


  method END_MEASUREMENT.
    FIELD-SYMBOLS: <lwa_measurement> TYPE t_measurement.

    DATA: l_time TYPE i.

    READ TABLE measurement_tab ASSIGNING <lwa_measurement>
                               WITH KEY id = pi_id.

    IF sy-subrc = 0.
      GET RUN TIME FIELD <lwa_measurement>-end_time.

      l_time = <lwa_measurement>-end_time - <lwa_measurement>-start_time.

      ADD l_time TO <lwa_measurement>-total_time.
      ADD 1 TO <lwa_measurement>-total_executions.
    ENDIF.
  endmethod.


  method HELPER_DISPLAY_GRAPHICAL.
    CONSTANTS: lc_bar_starting_x TYPE i VALUE 53.

    DATA: l_worst TYPE abap_bool,
          l_best TYPE abap_bool,
          l_worst_time TYPE i,
          l_best_time TYPE i,
          l_worst_time_pos TYPE i,
          l_best_time_pos TYPE i,

          l_percentage TYPE p DECIMALS 2,
          l_percentage_text TYPE c LENGTH 7,
          l_bar_position TYPE i,

          l_average_time TYPE i,

          lwa_measurement TYPE t_measurement,

          li_measurement TYPE STANDARD TABLE OF t_measurement.

    LOOP AT measurement_tab INTO lwa_measurement.
      IF lwa_measurement-total_time > l_worst_time.
        l_worst_time = lwa_measurement-total_time.
        l_worst_time_pos = lwa_measurement-position.
      ENDIF.

      IF lwa_measurement-total_time < l_best_time OR
         l_best_time IS INITIAL.
        l_best_time = lwa_measurement-total_time.
        l_best_time_pos = lwa_measurement-position.
      ENDIF.
    ENDLOOP.

    li_measurement[] = measurement_tab[].
    SORT li_measurement BY position.
    LOOP AT li_measurement INTO lwa_measurement.
      IF lwa_measurement-position = l_worst_time_pos.
        l_worst = abap_true.
        l_best = abap_false.
      ELSEIF lwa_measurement-position = l_best_time_pos..
        l_worst = abap_false.
        l_best = abap_true.
      ELSE.
        l_worst = abap_false.
        l_best = abap_false.
      ENDIF.

      FORMAT COLOR COL_BACKGROUND INTENSIFIED.
      WRITE: / lwa_measurement-id, '|'.

      l_percentage = lwa_measurement-total_time / l_worst_time * 100.
      l_bar_position = lc_bar_starting_x.
      DO 100 TIMES.
        IF sy-index <= l_percentage OR
           sy-index = 1.
          IF l_worst = abap_true.
            FORMAT COLOR COL_NEGATIVE.
          ELSEIF l_best = abap_true.
            FORMAT COLOR COL_POSITIVE.
          ELSE.
            FORMAT COLOR COL_GROUP.
          ENDIF.
        ELSE.
          FORMAT COLOR COL_BACKGROUND.
        ENDIF.

        WRITE AT l_bar_position(1) ' '.

        ADD 1 TO l_bar_position.
      ENDDO.
      FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
      WRITE: l_percentage TO l_percentage_text.
      CONCATENATE l_percentage_text '%' INTO l_percentage_text.
      IF pi_time_mode = e_time_mode-average.
        l_average_time = lwa_measurement-total_time / lwa_measurement-total_executions.
        WRITE: 'Average time:', l_average_time, 'ms', '(', l_percentage_text, ')'.
      ELSE.
        WRITE: lwa_measurement-total_time, 'ms', '(', l_percentage_text, ')'.
      ENDIF.
      WRITE: /4 'Total executions:', lwa_measurement-total_executions.
      WRITE AT 52 '|'.
    ENDLOOP.

    FORMAT COLOR COL_BACKGROUND INTENSIFIED.
    WRITE: /52 '|'.
    l_bar_position = lc_bar_starting_x - 1.
    DO 100 TIMES.
      WRITE AT l_bar_position(1) '-'.
      ADD 1 TO l_bar_position.
    ENDDO.
  endmethod.


  method HELPER_DISPLAY_TEXT.
    DATA: l_average_time TYPE i,

          lwa_measurement TYPE t_measurement,

          li_measurement TYPE STANDARD TABLE OF t_measurement.

    li_measurement[] = measurement_tab[].
    SORT li_measurement BY position.
    LOOP AT li_measurement INTO lwa_measurement.
      WRITE: / lwa_measurement-id, '(', lwa_measurement-total_executions, ' times executed):'.
      IF pi_time_mode = e_time_mode-average.
        l_average_time = lwa_measurement-total_time / lwa_measurement-total_executions.
        WRITE: 'Average time:', l_average_time, 'ms'.
      ELSE.
        WRITE: lwa_measurement-total_time, 'ms'.
      ENDIF.
    ENDLOOP.
  endmethod.


  method SET_CLOCK_RESOLUTION.
    CASE pi_clock_resolution.
      WHEN e_clock_resolution-low.
        SET RUN TIME CLOCK RESOLUTION LOW.
      WHEN e_clock_resolution-high.
        SET RUN TIME CLOCK RESOLUTION HIGH.
    ENDCASE.
  endmethod.


  method START_MEASUREMENT.
    FIELD-SYMBOLS: <lwa_measurement> TYPE t_measurement.

    DATA: l_lines TYPE i,

          lwa_measurement TYPE t_measurement.

    READ TABLE measurement_tab ASSIGNING <lwa_measurement>
                               WITH KEY id = pi_id.

    IF sy-subrc <> 0.
      DESCRIBE TABLE measurement_tab LINES l_lines.

      lwa_measurement-position = l_lines + 1.
      lwa_measurement-id = pi_id.
      INSERT lwa_measurement INTO TABLE measurement_tab ASSIGNING <lwa_measurement>.
    ENDIF.

    GET RUN TIME FIELD <lwa_measurement>-start_time.
  endmethod.
ENDCLASS.
