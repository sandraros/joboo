*"* use this source file for your ABAP unit test classes
class ltc_main definition
      for testing
      duration short
      risk level harmless
      inheriting from cl_aunit_assert.
  private section.
    METHODS immediately FOR TESTING RAISING cx_static_check.
    METHODS after_job FOR TESTING RAISING cx_static_check.
    METHODS external_program FOR TESTING RAISING cx_static_check.
    METHODS two_steps FOR TESTING RAISING cx_static_check.
    METHODS external_command FOR TESTING RAISING cx_static_check.
    METHODS start_in_10_seconds FOR TESTING RAISING cx_static_check.
    METHODS start_daily_at_10_am FOR TESTING RAISING cx_static_check.
    METHODS start_daily_except_holidays FOR TESTING RAISING cx_static_check.
    METHODS start_monthly_3rd_workday FOR TESTING RAISING cx_static_check.
    METHODS set_successor_job FOR TESTING RAISING cx_static_check.
*    class-methods class_setup.
*    class-methods class_teardown.
*    methods setup.
*    methods teardown.
endclass.
class ltc_main implementation.
  method immediately.
    zcl_job=>new( name = 'JOB NAME' )->add_step_abap( report = 'ZZTEST' )->start_immediately( ).
  endmethod.

  method after_job.
    " it may be more simple to use SET_SUCCESSOR_JOB
    data(job1) = zcl_job=>new( name = 'JOB NAME 2' )->add_step_abap( report = 'ZZTEST' ).
    zcl_job=>new( name = 'JOB NAME 1' )->add_step_abap( report = 'ZZTEST' )->start_after_job( job1 ).
    job1->start_immediately( ).
  endmethod.

  method SET_SUCCESSOR_JOB.
    " it's more simple than START_AFTER_JOB
    zcl_job=>new( name = 'JOB NAME 1' )->add_step_abap( report = 'ZZTEST_JOB1'
            )->set_successor_job( successor = zcl_job=>new( name = 'JOB NAME 2' )->add_step_abap( report = 'ZZTEST_JOB2' )
            )->start_immediately( ).
  endmethod.

  method external_program.
    zcl_job=>new( name = 'one step' )->add_step_external_program( program = 'cmd /c dir' wait_for_termination = abap_true )->start_immediately( ).
  ENDMETHOD.
  METHOD two_steps.
    zcl_job=>new( name = 'two steps' )->add_step_abap( report = sy-repid )->add_step_abap( report = sy-repid )->start_immediately( ).
  ENDMETHOD.
  METHOD external_command.
    zcl_job=>new( name = 'SM49' )->add_step_external_command( command = 'ZDIR' )->start_immediately( ).
  ENDMETHOD.
  METHOD start_in_10_seconds.
    zcl_job=>new( name = 'OS command without SM49' )->set_server( server = '' )->start_at( date = sy-datum time = sy-uzeit
*        not_later_than_date =
*        not_later_than_time =
).
  ENDMETHOD.
  METHOD start_daily_at_10_am.
  ENDMETHOD.
  METHOD start_daily_except_holidays.
  ENDMETHOD.
  METHOD start_monthly_3rd_workday  .
  endmethod.
endclass.
