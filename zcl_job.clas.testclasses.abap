*"* use this source file for your ABAP unit test classes
*class ltc_low_level_ops DEFINITION DEFERRED.
CLASS ltc_low_level_ops DEFINITION
*    INHERITING FROM lcl_low_level_ops
    FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_job_llop.
    METHODS constructor IMPORTING actual TYPE abap_bool.
*    METHODS zif_job_llop~job_open REDEFINITION.
*    METHODS zif_job_llop~abap_submit REDEFINITION.
*    METHODS zif_job_llop~job_submit REDEFINITION.
*    METHODS zif_job_llop~job_close REDEFINITION.
    ALIASES job_open FOR zif_job_llop~job_open.
    ALIASES job_submit FOR zif_job_llop~job_submit.
    ALIASES abap_submit FOR zif_job_llop~abap_submit.
    ALIASES job_close FOR zif_job_llop~job_close.
    TYPES : BEGIN OF ty_spy_parameter,
              name  TYPE string,
              value TYPE string,
            END OF ty_spy_parameter,
            ty_spy_parameters TYPE STANDARD TABLE OF ty_spy_parameter WITH EMPTY KEY,
            BEGIN OF ty_spy_line,
              func_name  TYPE string,
              parameters TYPE ty_spy_parameters,
            END OF ty_spy_line,
            ty_spy_lines TYPE STANDARD TABLE OF ty_spy_line WITH EMPTY KEY.
*    METHODS filter_out_initial_parameters
*      IMPORTING
*        spy_in         TYPE ty_spy_line
*      RETURNING
*        VALUE(spy_out) TYPE ty_spy_line.
    METHODS serialize
      IMPORTING
        data            TYPE any
      RETURNING
        VALUE(r_result) TYPE string.
    DATA: spy    TYPE ty_spy_lines READ-ONLY,
          actual TYPE abap_bool.

ENDCLASS.


CLASS ltc_low_level_ops IMPLEMENTATION.

  METHOD zif_job_llop~job_open.

    spy = VALUE #( BASE spy
        ( "filter_out_initial_parameters( VALUE #(
          func_name  = 'JOB_OPEN'
          parameters = VALUE #(
            ( name = 'JOBNAME'        value = jobname )
            ( name = 'JOBCLASS'       value = jobclass )
            ( name = 'CHECK_JOBCLASS' value = check_jobclass ) ) ) ).

*    IF actual = abap_true.
*      super->zif_job_llop~job_open(
*        EXPORTING
*          jobname        = jobname
*          jobclass       = jobclass
*          check_jobclass = check_jobclass
*        IMPORTING
*          jobcount       = jobcount
*          info           = info
*        CHANGING
*          ret            = ret
*      ).
**      CATCH zcx_job.    "
*    ENDIF.

  ENDMETHOD.

  METHOD zif_job_llop~abap_submit.

    spy = VALUE #( BASE spy
        ( "filter_out_initial_parameters( VALUE #(
          func_name  = 'ABAP_SUBMIT'
          parameters = VALUE #(
            ( name = 'JOBNAME  '       value = jobname )
            ( name = 'SELECTION_TABLE' value = serialize( selection_table ) )
            ( name = 'ARCPARAMS'       value = serialize( arcparams ) )
            ( name = 'USER     '       value = user      )
            ( name = 'PRIPARAMS'       value = serialize( priparams ) )
            ( name = 'REPORT   '       value = report    )
            ( name = 'VARIANT  '       value = variant   ) ) ) ).

*    IF actual = abap_true.
*      super->zif_job_llop~abap_submit(
*        EXPORTING
*          jobname         = jobname
*          jobcount        = jobcount
*          selection_table = selection_table
*          arcparams       = arcparams
*          user            = user
*          priparams       = priparams
*          report          = report
*          variant         = variant
*          this_procedure  = this_procedure
*      ).
**      CATCH zcx_job.    " .
*    ENDIF.

  ENDMETHOD.

  METHOD zif_job_llop~job_submit.

    spy = VALUE #( BASE spy
        ( "filter_out_initial_parameters( VALUE #(
          func_name  = 'JOB_SUBMIT'
          parameters = VALUE #(
            ( name = 'JOBNAME                    ' value = jobname                     )
            ( name = 'ARCPARAMS                  ' value = serialize( arcparams )      )
            ( name = 'AUTHCKNAM                  ' value = authcknam                   )
            ( name = 'COMMANDNAME                ' value = commandname                 )
            ( name = 'OPERATINGSYSTEM            ' value = operatingsystem             )
            ( name = 'EXTPGM_NAME                ' value = extpgm_name                 )
            ( name = 'EXTPGM_PARAM               ' value = extpgm_param                )
            ( name = 'EXTPGM_SET_TRACE_ON        ' value = extpgm_set_trace_on         )
            ( name = 'EXTPGM_STDERR_IN_JOBLOG    ' value = extpgm_stderr_in_joblog     )
            ( name = 'EXTPGM_STDOUT_IN_JOBLOG    ' value = extpgm_stdout_in_joblog     )
            ( name = 'EXTPGM_SYSTEM              ' value = extpgm_system               )
            ( name = 'EXTPGM_RFCDEST             ' value = extpgm_rfcdest              )
            ( name = 'EXTPGM_WAIT_FOR_TERMINATION' value = extpgm_wait_for_termination )
            ( name = 'LANGUAGE                   ' value = language                    )
            ( name = 'PRIPARAMS                  ' value = serialize( priparams )      )
            ( name = 'REPORT                     ' value = report                      )
            ( name = 'VARIANT                    ' value = variant                     ) ) ) ).

*    IF actual = abap_true.
*      super->zif_job_llop~job_submit(
*        EXPORTING
*          jobname                     = jobname
*          jobcount                    = jobcount
*          arcparams                   = arcparams
*          authcknam                   = authcknam
*          commandname                 = commandname
*          operatingsystem             = operatingsystem
*          extpgm_name                 = extpgm_name
*          extpgm_param                = extpgm_param
*          extpgm_set_trace_on         = extpgm_set_trace_on
*          extpgm_stderr_in_joblog     = extpgm_stderr_in_joblog
*          extpgm_stdout_in_joblog     = extpgm_stdout_in_joblog
*          extpgm_system               = extpgm_system
*          extpgm_rfcdest              = extpgm_rfcdest
*          extpgm_wait_for_termination = extpgm_wait_for_termination
*          language                    = language
*          priparams                   = priparams
*          report                      = report
*          variant                     = variant
*          this_procedure              = this_procedure
*        IMPORTING
*          step_number                 = step_number
*      ).
**      CATCH zcx_job.    " (
*    ENDIF.

  ENDMETHOD.

  METHOD zif_job_llop~job_close.

    spy = VALUE #( BASE spy
        ( "filter_out_initial_parameters( VALUE #(
          func_name  = 'JOB_CLOSE'
          parameters = VALUE #(
            ( name = 'JOBNAME                    ' value = jobname                     )
            ( name = 'AT_OPMODE                  ' value = at_opmode                   )
            ( name = 'AT_OPMODE_PERIODIC         ' value = at_opmode_periodic          )
            ( name = 'EVENT_ID                   ' value = event_id                    )
            ( name = 'EVENT_PARAM                ' value = event_param                 )
            ( name = 'EVENT_PERIODIC             ' value = event_periodic              )
            ( name = 'SDLSTRTDT                  ' value = sdlstrtdt                   )
            ( name = 'SDLSTRTTM                  ' value = sdlstrttm                   )
            ( name = 'LASTSTRTDT                 ' value = laststrtdt                  )
            ( name = 'LASTSTRTTM                 ' value = laststrttm                  )
            ( name = 'PRDDAYS                    ' value = prddays                     )
            ( name = 'PRDHOURS                   ' value = prdhours                    )
            ( name = 'PRDMINS                    ' value = prdmins                     )
            ( name = 'PRDMONTHS                  ' value = prdmonths                   )
            ( name = 'PRDWEEKS                   ' value = prdweeks                    )
            ( name = 'CALENDAR_ID                ' value = calendar_id                 )
            ( name = 'STARTDATE_RESTRICTION      ' value = startdate_restriction       )
            ( name = 'START_ON_WORKDAY_NOT_BEFORE' value = start_on_workday_not_before )
            ( name = 'START_ON_WORKDAY_NR        ' value = start_on_workday_nr         )
            ( name = 'WORKDAY_COUNT_DIRECTION    ' value = workday_count_direction     )
            ( name = 'PREDJOB_CHECKSTAT          ' value = predjob_checkstat           )
            ( name = 'PRED_JOBCOUNT              ' value = pred_jobcount               )
            ( name = 'PRED_JOBNAME               ' value = pred_jobname                )
            ( name = 'STRTIMMED                  ' value = strtimmed                   )
            ( name = 'DIRECT_START               ' value = direct_start                )
            ( name = 'RECIPIENT_OBJ              ' value = recipient_obj               )
            ( name = 'TARGETSYSTEM               ' value = targetsystem                )
            ( name = 'TARGETSERVER               ' value = targetserver                )
            ( name = 'TARGETGROUP                ' value = targetgroup                 )
            ( name = 'DONT_RELEASE               ' value = dont_release                ) ) ) ).

job_was_released = abap_true.

*    IF actual = abap_true.
*      super->zif_job_llop~job_close(
*        EXPORTING
*          jobname                     = jobname
*          jobcount                    = jobcount
*          at_opmode                   = at_opmode
*          at_opmode_periodic          = at_opmode_periodic
*          event_id                    = event_id
*          event_param                 = event_param
*          event_periodic              = event_periodic
*          sdlstrtdt                   = sdlstrtdt
*          sdlstrttm                   = sdlstrttm
*          laststrtdt                  = laststrtdt
*          laststrttm                  = laststrttm
*          prddays                     = prddays
*          prdhours                    = prdhours
*          prdmins                     = prdmins
*          prdmonths                   = prdmonths
*          prdweeks                    = prdweeks
*          calendar_id                 = calendar_id
*          startdate_restriction       = startdate_restriction
*          start_on_workday_not_before = start_on_workday_not_before
*          start_on_workday_nr         = start_on_workday_nr
*          workday_count_direction     = workday_count_direction
*          predjob_checkstat           = predjob_checkstat
*          pred_jobcount               = pred_jobcount
*          pred_jobname                = pred_jobname
*          strtimmed                   = strtimmed
*          direct_start                = direct_start
*          recipient_obj               = recipient_obj
*          targetsystem                = targetsystem
*          targetserver                = targetserver
*          targetgroup                 = targetgroup
*          dont_release                = dont_release
*          CHANGING
*          ret = ret
*      ).
**      CATCH zcx_job.    "
*    ENDIF.

  ENDMETHOD.

*  METHOD filter_out_initial_parameters.
*
*    spy_out = spy_in.
*    DELETE spy_out-parameters WHERE value IS INITIAL OR value CO '0'.
*
*  ENDMETHOD.

  METHOD serialize.

    DATA(writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id
        SOURCE data = data
        RESULT XML writer
        OPTIONS initial_components = 'suppress'.
    r_result = writer->get_output( ).

  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    me->actual = actual.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_main DEFINITION DEFERRED.
CLASS zcl_job DEFINITION LOCAL FRIENDS ltc_main.

CLASS ltc_main DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS start_immediately            FOR TESTING RAISING zcx_job.
    METHODS two_steps                    FOR TESTING RAISING zcx_job.
    METHODS external_command_immediately FOR TESTING RAISING zcx_job.
    METHODS external_program_immediately FOR TESTING RAISING zcx_job.
    METHODS start_in_10_seconds          FOR TESTING RAISING zcx_job.
    METHODS start_daily_at_10_am         FOR TESTING RAISING zcx_job.
    METHODS start_daily_except_holidays  FOR TESTING RAISING zcx_job.
    METHODS start_monthly_3rd_workday    FOR TESTING RAISING zcx_job.

    METHODS setup.
    METHODS serialize
      IMPORTING
        data            TYPE any
      RETURNING
        VALUE(r_result) TYPE string.
    DATA actual TYPE REF TO ltc_low_level_ops.
    DATA expected TYPE REF TO ltc_low_level_ops.
    DATA job TYPE REF TO zif_job.
    DATA lx_job TYPE REF TO zcx_job.
    CONSTANTS c_demo_report TYPE syrepid VALUE 'Z_DEMO_JOBOO' ##NO_TEXT.

ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD setup.

    actual = NEW ltc_low_level_ops( actual = abap_true ).
    zcl_job=>inject_td( actual ).

    expected = NEW ltc_low_level_ops( actual = abap_false ).

  ENDMETHOD.

  METHOD start_immediately.

    job = zcl_job=>new( name = 'one step' ).
    job->add_step_abap( report = c_demo_report ).
    job->start_immediately( ).

    expected->job_open( jobname = job->name jobclass = space ).
    expected->job_submit( jobname = job->name jobcount = job->count report = c_demo_report ).
    expected->job_close( jobname = job->name jobcount = job->count strtimmed = 'X' ).
    cl_abap_unit_assert=>assert_equals( act = actual->spy exp = expected->spy ).

  ENDMETHOD.

  METHOD two_steps.

    job = zcl_job=>new( name = 'two steps' ).
    job->add_step_abap( report = c_demo_report ).
    job->add_step_abap( report = c_demo_report ).
    job->start_immediately( ).

    expected->job_open( jobname = job->name jobclass = space ).
    expected->job_submit( jobname = job->name jobcount = job->count report = c_demo_report ).
    expected->job_submit( jobname = job->name jobcount = job->count report = c_demo_report ).
    expected->job_close( jobname = job->name jobcount = job->count strtimmed = 'X' ).
    cl_abap_unit_assert=>assert_equals( act = actual->spy exp = expected->spy ).

  ENDMETHOD.

  METHOD external_command_immediately.

    job = zcl_job=>new( name = 'SM49' ).
    job->add_step_external_command( command = 'ZDIR' ).
    job->start_immediately( ).

    expected->job_open( jobname = job->name jobclass = space ).
    expected->job_submit( jobname = job->name jobcount = job->count commandname = 'ZDIR' ).
    expected->job_close( jobname = job->name jobcount = job->count strtimmed = 'X' ).
    cl_abap_unit_assert=>assert_equals( act = actual->spy exp = expected->spy ).

  ENDMETHOD.

  METHOD external_program_immediately.

    job = zcl_job=>new( name = 'OS command without SM49' ).
    job->add_step_external_program( program = 'cmd /c dir' wait_for_termination = abap_true ).
    job->start_immediately( ).

  ENDMETHOD.

  METHOD start_in_10_seconds.

    job = zcl_job=>new( name = 'in 10 seconds' ).
    job->add_step_abap( report = c_demo_report ).
    " let's hope the test doesn't run at midnight (rare and acceptable false positive)
    job->start_at( date = sy-datum time = CONV #( sy-uzeit + 10 ) ).

  ENDMETHOD.

  METHOD start_daily_at_10_am.

    job = zcl_job=>new( name = 'daily at 10 AM' ).
    job->add_step_abap( report = sy-repid ).
    job->start_periodically( first_date = CONV #( sy-datum + 1 ) first_time = '100000' days = 1 ).

  ENDMETHOD.

  METHOD start_daily_except_holidays.

    job = zcl_job=>new( name = 'daily except holidays' ).
    job->start_periodically(
            first_date = CONV #( sy-datum + 1 ) first_time = '100000'
            days = 1
            calendar_id = 'FR'
            rule_if_date_falls_on_holiday = zcl_job=>calendar_rule-process_always ).

  ENDMETHOD.

  METHOD start_monthly_3rd_workday  .

    job = zcl_job=>new( name = 'daily except holidays' ).
    job->add_step_abap( report = sy-repid ).
    job->start_monthly_nth_workday(
            first_date  = CONV #( sy-datum + 1 )
            first_time  = '100000'
            months      = 1
            calendar_id = 'FR'
            nth_workday = 3 ).

  ENDMETHOD.

  METHOD serialize.

    DATA(writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id
        SOURCE data = data
        RESULT XML writer
        OPTIONS initial_components = 'suppress'.
    r_result = writer->get_output( ).

  ENDMETHOD.

ENDCLASS.
