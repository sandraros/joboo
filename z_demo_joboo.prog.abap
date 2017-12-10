REPORT.


CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
    CLASS-METHODS simple_immediately          RAISING zcx_job.
    CLASS-METHODS two_steps_immediately       RAISING zcx_job.
    CLASS-METHODS external_command_immediately RAISING zcx_job.
    CLASS-METHODS external_program_immediately RAISING zcx_job.
    CLASS-METHODS start_in_10_seconds         RAISING zcx_job.
    CLASS-METHODS start_daily_at_10_am        RAISING zcx_job.
    CLASS-METHODS start_daily_except_holidays RAISING zcx_job.
    CLASS-METHODS start_monthly_3rd_workday   RAISING zcx_job.
  PRIVATE SECTION.
    CLASS-DATA job TYPE REF TO zcl_job.
    CLASS-DATA lx_job TYPE REF TO zcx_job.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    TRY.
*        simple_immediately( ).
*        two_steps_immediately( ).
*        external_command_immediately( ).
        external_program_immediately( ).
        start_in_10_seconds( ).
        start_daily_at_10_am( ).
        start_daily_except_holidays( ).
        start_monthly_3rd_workday( ).
      CATCH zcx_job INTO lx_job.
        MESSAGE lx_job TYPE 'I'.
    ENDTRY.
  ENDMETHOD.
  METHOD simple_immediately.
    CREATE OBJECT job
      EXPORTING
        name = 'one step'.
    job->add_step_abap( report = sy-repid ).
    job->start_immediately( ).
  ENDMETHOD.
  METHOD two_steps_immediately.
    CREATE OBJECT job
      EXPORTING
        name = 'two steps'.
    job->add_step_abap( report = sy-repid ).
    job->add_step_abap( report = sy-repid ).
    job->start_immediately( ).
  ENDMETHOD.
  METHOD external_command_immediately.
    CREATE OBJECT job
      EXPORTING
        name = 'SM49'.
    job->add_step_external_command( command = 'ZDIR' ).
    job->start_immediately( ).
  ENDMETHOD.
  METHOD external_program_immediately.
    CREATE OBJECT job
      EXPORTING
        name = 'OS command without SM49'.
    job->add_step_external_program( program = 'cmd /c dir' wait_for_termination = abap_true ).
    job->start_immediately( ).
  ENDMETHOD.
  METHOD start_in_10_seconds.
    CREATE OBJECT job
      EXPORTING
        name = 'OS command without SM49'.
  ENDMETHOD.
  METHOD start_daily_at_10_am.
  ENDMETHOD.
  METHOD start_daily_except_holidays.
  ENDMETHOD.
  METHOD start_monthly_3rd_workday  .
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  CHECK sy-batch = abap_false.
  lcl_app=>main( ).
