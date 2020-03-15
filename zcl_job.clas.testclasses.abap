*"* use this source file for your ABAP unit test classes
*class ltc_low_level_ops DEFINITION DEFERRED.
CLASS ltc_main DEFINITION DEFERRED.
CLASS zcl_job DEFINITION LOCAL FRIENDS ltc_main.

CLASS ltc_low_level_ops DEFINITION
    INHERITING FROM lcl_low_level_ops
    FOR TESTING.
  PUBLIC SECTION.
    METHODS zif_job_llop~job_open REDEFINITION.
    METHODS zif_job_llop~abap_submit REDEFINITION.
    METHODS zif_job_llop~job_submit REDEFINITION.
    METHODS zif_job_llop~job_close REDEFINITION.
    TYPES : BEGIN OF ty_spy_parameter,
              name  TYPE string,
              value TYPE string,
            END OF ty_spy_parameter,
            BEGIN OF ty_spy_line,
              func_name  TYPE string,
              parameters TYPE STANDARD TABLE OF ty_spy_parameter WITH EMPTY KEY,
            END OF ty_spy_line,
            ty_spy_lines TYPE STANDARD TABLE OF ty_spy_line WITH EMPTY KEY.
    METHODS filter_out_initial_parameters
      IMPORTING
        spy_in         TYPE ty_spy_line
      RETURNING
        VALUE(spy_out) TYPE ty_spy_line.
    METHODS serialize
      IMPORTING
        data            TYPE any
      RETURNING
        VALUE(r_result) TYPE string.
    DATA spy TYPE ty_spy_lines READ-ONLY.

ENDCLASS.


CLASS ltc_low_level_ops IMPLEMENTATION.

  METHOD zif_job_llop~job_open.

    spy = VALUE #( BASE spy
        ( filter_out_initial_parameters( VALUE #(
          func_name  = 'JOB_OPEN'
          parameters = VALUE #(
            ( name = 'JOBNAME'        value = jobname )
            ( name = 'JOBCLASS'       value = jobclass )
            ( name = 'CHECK_JOBCLASS' value = check_jobclass ) ) ) ) ) ).

    super->zif_job_llop~job_open(
      EXPORTING
        jobname        = jobname
        jobclass       = jobclass
        check_jobclass = check_jobclass
      IMPORTING
        jobcount       = jobcount
        info           = info
      CHANGING
        ret            = ret
    ).
*      CATCH zcx_job.    "

  ENDMETHOD.

  METHOD zif_job_llop~abap_submit.

    spy = VALUE #( BASE spy
        ( filter_out_initial_parameters( VALUE #(
          func_name  = 'ABAP_SUBMIT'
          parameters = VALUE #(
            ( name = 'JOBNAME  '       value = jobname )
            ( name = 'SELECTION_TABLE' value = serialize( selection_table ) )
            ( name = 'ARCPARAMS'       value = serialize( arcparams ) )
            ( name = 'USER     '       value = user      )
            ( name = 'PRIPARAMS'       value = serialize( priparams ) )
            ( name = 'REPORT   '       value = report    )
            ( name = 'VARIANT  '       value = variant   ) ) ) ) ) ).

    super->zif_job_llop~abap_submit(
      EXPORTING
        jobname         = jobname
        jobcount        = jobcount
        selection_table = selection_table
        arcparams       = arcparams
        user            = user
        priparams       = priparams
        report          = report
        variant         = variant
        this_routine    = this_routine
    ).
*      CATCH zcx_job.    " .

  ENDMETHOD.

  METHOD zif_job_llop~job_submit.

    spy = VALUE #( BASE spy
        ( filter_out_initial_parameters( VALUE #(
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
            ( name = 'VARIANT                    ' value = variant                     ) ) ) ) ) ).

    super->zif_job_llop~job_submit(
      EXPORTING
        jobname                     = jobname
        jobcount                    = jobcount
        arcparams                   = arcparams
        authcknam                   = authcknam
        commandname                 = commandname
        operatingsystem             = operatingsystem
        extpgm_name                 = extpgm_name
        extpgm_param                = extpgm_param
        extpgm_set_trace_on         = extpgm_set_trace_on
        extpgm_stderr_in_joblog     = extpgm_stderr_in_joblog
        extpgm_stdout_in_joblog     = extpgm_stdout_in_joblog
        extpgm_system               = extpgm_system
        extpgm_rfcdest              = extpgm_rfcdest
        extpgm_wait_for_termination = extpgm_wait_for_termination
        language                    = language
        priparams                   = priparams
        report                      = report
        variant                     = variant
        this_routine                = this_routine
      IMPORTING
        step_number                 = step_number
    ).
*      CATCH zcx_job.    " (

  ENDMETHOD.

  METHOD zif_job_llop~job_close.

    spy = VALUE #( BASE spy
        ( filter_out_initial_parameters( VALUE #(
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
            ( name = 'DONT_RELEASE               ' value = dont_release                ) ) ) ) ) ).

    super->zif_job_llop~job_close(
      EXPORTING
        jobname                     = jobname
        jobcount                    = jobcount
        at_opmode                   = at_opmode
        at_opmode_periodic          = at_opmode_periodic
        event_id                    = event_id
        event_param                 = event_param
        event_periodic              = event_periodic
        sdlstrtdt                   = sdlstrtdt
        sdlstrttm                   = sdlstrttm
        laststrtdt                  = laststrtdt
        laststrttm                  = laststrttm
        prddays                     = prddays
        prdhours                    = prdhours
        prdmins                     = prdmins
        prdmonths                   = prdmonths
        prdweeks                    = prdweeks
        calendar_id                 = calendar_id
        startdate_restriction       = startdate_restriction
        start_on_workday_not_before = start_on_workday_not_before
        start_on_workday_nr         = start_on_workday_nr
        workday_count_direction     = workday_count_direction
        predjob_checkstat           = predjob_checkstat
        pred_jobcount               = pred_jobcount
        pred_jobname                = pred_jobname
        strtimmed                   = strtimmed
        direct_start                = direct_start
        recipient_obj               = recipient_obj
        targetsystem                = targetsystem
        targetserver                = targetserver
        targetgroup                 = targetgroup
        dont_release                = dont_release
    ).
*      CATCH zcx_job.    "

  ENDMETHOD.

  METHOD filter_out_initial_parameters.

    spy_out = spy_in.
    DELETE spy_out-parameters WHERE value IS INITIAL OR value CO '0'.

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

CLASS ltc_main DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    METHODS sTART_immediately          FOR TESTING RAISING zcx_job.
    METHODS two_steps_immediately       FOR TESTING RAISING zcx_job.
    METHODS external_command_immediately FOR TESTING RAISING zcx_job.
    METHODS external_program_immediately FOR TESTING RAISING zcx_job.
    METHODS start_in_10_seconds         FOR TESTING RAISING zcx_job.
    METHODS start_daily_at_10_am        FOR TESTING RAISING zcx_job.
    METHODS start_daily_except_holidays FOR TESTING RAISING zcx_job.
    METHODS start_monthly_3rd_workday   FOR TESTING RAISING zcx_job.
  PRIVATE SECTION.
    METHODS setup.
    METHODS serialize
      IMPORTING
        data            TYPE any
      RETURNING
        VALUE(r_result) TYPE string.
    DATA low_level_ops TYPE REF TO ltc_low_level_ops.
    DATA job TYPE REF TO zcl_job.
    DATA lx_job TYPE REF TO zcx_job.
    constants C_demo_program type syrepid value 'Z_DEMO_JOBOO' ##NO_TEXT.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD setup.
    low_level_ops = NEW ltc_low_level_ops( ).
    zcl_job=>inject_td( low_level_ops ).
  ENDMETHOD.

  METHOD sTART_immediately.

    zcl_job=>new( name = 'one step'
        )->add_step_abap( report = C_demo_program
        )->start_immediately( ).

    cl_abap_unit_assert=>assert_equals(
        act = low_level_ops->spy
        exp = VALUE low_level_ops->ty_spy_lines(
            ( func_name  = 'JOB_OPEN'
              parameters = VALUE #(
                ( name = 'JOBNAME' value = 'one step' ) ) )
            ( func_name  = 'JOB_SUBMIT'
              parameters = VALUE #(
                ( name = 'JOBNAME                    ' value = 'one step' )
                ( name = 'ARCPARAMS                  ' value = serialize( VALUE arc_params( ) ) )
                ( name = 'AUTHCKNAM                  ' value = sy-uname                   )
                ( name = 'EXTPGM_STDERR_IN_JOBLOG    ' value = 'X' )
                ( name = 'EXTPGM_STDOUT_IN_JOBLOG    ' value = 'X' )
                ( name = 'LANGUAGE                   ' value = sy-langu )
                ( name = 'PRIPARAMS                  ' value = serialize( VALUE pri_params( ) ) )
                ( name = 'REPORT                     ' value = C_demo_program ) ) )
            ( func_name  = 'JOB_CLOSE'
              parameters = VALUE #(
                ( name = 'JOBNAME' value = 'one step' )
                ( name = 'SDLSTRTTM' value = ' 00000' )
                ( name = 'STRTIMMED' value = 'X' ) ) ) ) ).
  ENDMETHOD.

  METHOD two_steps_immediately.

    zcl_job=>new( name = 'two steps'
        )->add_step_abap( report = sy-repid
        )->add_step_abap( report = sy-repid
        )->start_immediately( ).

  ENDMETHOD.

  METHOD external_command_immediately.
    zcl_job=>new( name = 'SM49'
        )->add_step_external_command( command = 'ZDIR'
        )->start_immediately( ).
  ENDMETHOD.

  METHOD external_program_immediately.
    zcl_job=>new( name = 'OS command without SM49'
        )->add_step_external_program( program = 'cmd /c dir' wait_for_termination = abap_true
        )->start_immediately( ).
  ENDMETHOD.

  METHOD start_in_10_seconds.
    zcl_job=>new( name = 'in 10 seconds'
        )->add_step_abap( report = sy-repid
        )->start_at( date = sy-datum time = CONV #( sy-uzeit + 10 ) ).
  ENDMETHOD.

  METHOD start_daily_at_10_am.
    zcl_job=>new( name = 'daily at 10 AM'
        )->add_step_abap( report = sy-repid
        )->start_periodically( first_date = CONV #( sy-datum + 1 ) first_time = '100000' days = 1 ).
  ENDMETHOD.

  METHOD start_daily_except_holidays.
    zcl_job=>new( name = 'daily except holidays'
        )->start_periodically(
            first_date = CONV #( sy-datum + 1 ) first_time = '100000'
            days = 1
            calendar_id = 'FR'
            rule_if_date_falls_on_holiday = zcl_job=>calendar_rule-process_always ).
  ENDMETHOD.

  METHOD start_monthly_3rd_workday  .
    zcl_job=>new( name = 'daily except holidays'
        )->add_step_abap( report = sy-repid
        )->start_monthly_nth_workday(
            first_date = CONV #( sy-datum + 1 ) first_time = '100000'
            months = 1
            calendar_id = 'FR' nth_workday = 3 ).
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
