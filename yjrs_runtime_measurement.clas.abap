class YJRS_RUNTIME_MEASUREMENT definition
  public
  final
  create public .

public section.

  types:
    T_MEASUREMENT_ID TYPE c LENGTH 50 .

  class-methods START_MEASUREMENT
    importing
      !PI_ID type T_MEASUREMENT_ID .
  class-methods END_MEASUREMENT
    importing
      !PI_ID type T_MEASUREMENT_ID .
  class-methods DISPLAY_MEASUREMENT .
protected section.
private section.

  types:
    BEGIN OF T_MEASUREMENT,
           id TYPE t_measurement_id,
           start_time TYPE i,
           end_time TYPE i,
           total_time TYPE i,
           total_executions TYPE i,
         END OF T_MEASUREMENT .
  types:
    t_measurement_tab TYPE SORTED TABLE OF t_measurement WITH UNIQUE KEY id .

  class-data MEASUREMENT_TAB type T_MEASUREMENT_TAB .
ENDCLASS.



CLASS YJRS_RUNTIME_MEASUREMENT IMPLEMENTATION.


  method DISPLAY_MEASUREMENT.
    DATA: lwa_measurement TYPE t_measurement.

    LOOP AT measurement_tab INTO lwa_measurement.
      WRITE: / lwa_measurement-id, '(', lwa_measurement-total_executions, ' times executed):', lwa_measurement-total_time, 'ms'.
    ENDLOOP.
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


  method START_MEASUREMENT.
    FIELD-SYMBOLS: <lwa_measurement> TYPE t_measurement.

    DATA: lwa_measurement TYPE t_measurement.

    READ TABLE measurement_tab ASSIGNING <lwa_measurement>
                               WITH KEY id = pi_id.

    IF sy-subrc <> 0.
      lwa_measurement-id = pi_id.
      INSERT lwa_measurement INTO TABLE measurement_tab ASSIGNING <lwa_measurement>.
    ENDIF.

    GET RUN TIME FIELD <lwa_measurement>-start_time.
  endmethod.
ENDCLASS.
