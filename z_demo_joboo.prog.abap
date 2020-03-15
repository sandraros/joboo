*********************************************************************************
**  MIT License
**
**  Copyright (c) 2018 sandraros
**
**  Permission is hereby granted, free of charge, to any person obtaining a copy
**  of this software and associated documentation files (the "Software"), to deal
**  in the Software without restriction, including without limitation the rights
**  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
**  copies of the Software, and to permit persons to whom the Software is
**  furnished to do so, subject to the following conditions:
**
**  The above copyright notice and this permission notice shall be included in all
**  copies or substantial portions of the Software.
**
**  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
**  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
**  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
**  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
**  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
**  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
**  SOFTWARE.
*********************************************************************************
REPORT.
*
*
*CLASS lcl_app DEFINITION.
*  PUBLIC SECTION.
*    INTERFACES zif_job_td.
*    METHODS main IMPORTING it_sel TYPE rsparams_tt.
*    METHODS simple_immediately          RAISING zcx_job.
*    METHODS two_steps_immediately       RAISING zcx_job.
*    METHODS external_command_immediately RAISING zcx_job.
*    METHODS external_program_immediately RAISING zcx_job.
*    METHODS start_in_10_seconds         RAISING zcx_job.
*    METHODS start_daily_at_10_am        RAISING zcx_job.
*    METHODS start_daily_except_holidays RAISING zcx_job.
*    METHODS start_monthly_3rd_workday   RAISING zcx_job.
*  PRIVATE SECTION.
*    TYPES : BEGIN OF ty_spy_parameter,
*              name  TYPE string,
*              value TYPE string,
*            END OF ty_spy_parameter,
*            BEGIN OF ty_spy_line,
*              func_name  TYPE string,
*              parameters TYPE STANDARD TABLE OF ty_spy_parameter WITH EMPTY KEY,
*            END OF ty_spy_line,
*            ty_spy_lines TYPE STANDARD TABLE OF ty_spy_line WITH EMPTY KEY.
*    DATA spy TYPE ty_spy_lines.
*    DATA job TYPE REF TO zcl_job.
*    DATA real TYPE REF TO zif_job_td.
*    DATA lx_job TYPE REF TO zcx_job.
*    METHODS filter_out_initial_parameters
*      IMPORTING
*        spy_in         TYPE ty_spy_line
*      RETURNING
*        VALUE(spy_out) TYPE ty_spy_line.
*    METHODS serialize
*      IMPORTING
*        data            TYPE any
*      RETURNING
*        VALUE(r_result) TYPE string.
*ENDCLASS.
*
*CLASS lcl_app IMPLEMENTATION.
*
*  METHOD main.
*
*    LOOP AT it_sel REFERENCE INTO DATA(sel) WHERE low = 'X'.
*      TRY.
*          CASE sel->selname.
*            WHEN 'AFTERJOB' ##NO_TEXT.
*              simple_immediately( ).
*            WHEN 'IMMED' ##NO_TEXT.
*              two_steps_immediately( ).
*            WHEN 'AT_TIME' ##NO_TEXT.
*              start_in_10_seconds( ).
**            WHEN '' ##NO_TEXT.
**              external_command_immediately( ).
**            WHEN '' ##NO_TEXT.
**              external_program_immediately( ).
**            WHEN '' ##NO_TEXT.
**              start_daily_at_10_am( ).
**            WHEN '' ##NO_TEXT.
**              start_daily_except_holidays( ).
**            WHEN '' ##NO_TEXT.
**              start_monthly_3rd_workday( ).
*          ENDCASE.
*        CATCH zcx_job INTO lx_job.
*          MESSAGE lx_job TYPE 'I'.
*      ENDTRY.
*    ENDLOOP.
*
*  ENDMETHOD.
*
*  METHOD zif_job_td~_job_open.
*
*    spy = VALUE #( BASE spy
*        ( filter_out_initial_parameters( VALUE #(
*          func_name  = 'JOB_OPEN'
*          parameters = VALUE #(
*            ( name = 'NAME'           value = jobname )
*            ( name = 'JOBCOUNT '      value = jobcount )
*            ( name = 'CHECK_JOBCLASS' value = check_jobclass ) ) ) ) ) ).
*
*    real->_job_open(
*      EXPORTING
*        jobname        = jobname
*        jobclass       = jobclass
*        check_jobclass = check_jobclass
*      IMPORTING
*        jobcount       = jobcount
*        info           = info
*      CHANGING
*        ret            = ret
*    ).
**      CATCH zcx_job.    "
*
*  ENDMETHOD.
*
*  METHOD zif_job_td~_abap_submit.
*
*    spy = VALUE #( BASE spy
*        ( filter_out_initial_parameters( VALUE #(
*          func_name  = 'ABAP_SUBMIT'
*          parameters = VALUE #(
*            ( name = 'JOBNAME  '       value = jobname )
*            ( name = 'JOBCOUNT '       value = jobcount )
*            ( name = 'SELECTION_TABLE' value = serialize( selection_table ) )
*            ( name = 'ARCPARAMS'       value = serialize( arcparams ) )
*            ( name = 'USER     '       value = user      )
*            ( name = 'PRIPARAMS'       value = serialize( priparams ) )
*            ( name = 'REPORT   '       value = report    )
*            ( name = 'VARIANT  '       value = variant   ) ) ) ) ) ).
*
*    real->_abap_submit(
*      EXPORTING
*        jobname         = jobname
*        jobcount        = jobcount
*        selection_table = selection_table
*        arcparams       = arcparams
*        user            = user
*        priparams       = priparams
*        report          = report
*        variant         = variant
*        this_routine    = this_routine
*    ).
**      CATCH zcx_job.    " .
*
*  ENDMETHOD.
*
*  METHOD zif_job_td~_job_close.
*
*    spy = VALUE #( BASE spy
*        ( filter_out_initial_parameters( VALUE #(
*          func_name  = 'JOB_CLOSE'
*          parameters = VALUE #(
*            ( name = 'JOBNAME                    ' value = jobname                     )
*            ( name = 'JOBCOUNT                   ' value = jobcount                    )
*            ( name = 'AT_OPMODE                  ' value = at_opmode                   )
*            ( name = 'AT_OPMODE_PERIODIC         ' value = at_opmode_periodic          )
*            ( name = 'EVENT_ID                   ' value = event_id                    )
*            ( name = 'EVENT_PARAM                ' value = event_param                 )
*            ( name = 'EVENT_PERIODIC             ' value = event_periodic              )
*            ( name = 'SDLSTRTDT                  ' value = sdlstrtdt                   )
*            ( name = 'SDLSTRTTM                  ' value = sdlstrttm                   )
*            ( name = 'LASTSTRTDT                 ' value = laststrtdt                  )
*            ( name = 'LASTSTRTTM                 ' value = laststrttm                  )
*            ( name = 'PRDDAYS                    ' value = prddays                     )
*            ( name = 'PRDHOURS                   ' value = prdhours                    )
*            ( name = 'PRDMINS                    ' value = prdmins                     )
*            ( name = 'PRDMONTHS                  ' value = prdmonths                   )
*            ( name = 'PRDWEEKS                   ' value = prdweeks                    )
*            ( name = 'CALENDAR_ID                ' value = calendar_id                 )
*            ( name = 'STARTDATE_RESTRICTION      ' value = startdate_restriction       )
*            ( name = 'START_ON_WORKDAY_NOT_BEFORE' value = start_on_workday_not_before )
*            ( name = 'START_ON_WORKDAY_NR        ' value = start_on_workday_nr         )
*            ( name = 'WORKDAY_COUNT_DIRECTION    ' value = workday_count_direction     )
*            ( name = 'PREDJOB_CHECKSTAT          ' value = predjob_checkstat           )
*            ( name = 'PRED_JOBCOUNT              ' value = pred_jobcount               )
*            ( name = 'PRED_JOBNAME               ' value = pred_jobname                )
*            ( name = 'STRTIMMED                  ' value = strtimmed                   )
*            ( name = 'DIRECT_START               ' value = direct_start                )
*            ( name = 'RECIPIENT_OBJ              ' value = recipient_obj               )
*            ( name = 'TARGETSYSTEM               ' value = targetsystem                )
*            ( name = 'TARGETSERVER               ' value = targetserver                )
*            ( name = 'TARGETGROUP                ' value = targetgroup                 )
*            ( name = 'DONT_RELEASE               ' value = dont_release                ) ) ) ) ) ).
*
*    real->_job_close(
*      EXPORTING
*        jobname                     = jobname
*        jobcount                    = jobcount
*        at_opmode                   = at_opmode
*        at_opmode_periodic          = at_opmode_periodic
*        event_id                    = event_id
*        event_param                 = event_param
*        event_periodic              = event_periodic
*        sdlstrtdt                   = sdlstrtdt
*        sdlstrttm                   = sdlstrttm
*        laststrtdt                  = laststrtdt
*        laststrttm                  = laststrttm
*        prddays                     = prddays
*        prdhours                    = prdhours
*        prdmins                     = prdmins
*        prdmonths                   = prdmonths
*        prdweeks                    = prdweeks
*        calendar_id                 = calendar_id
*        startdate_restriction       = startdate_restriction
*        start_on_workday_not_before = start_on_workday_not_before
*        start_on_workday_nr         = start_on_workday_nr
*        workday_count_direction     = workday_count_direction
*        predjob_checkstat           = predjob_checkstat
*        pred_jobcount               = pred_jobcount
*        pred_jobname                = pred_jobname
*        strtimmed                   = strtimmed
*        direct_start                = direct_start
*        recipient_obj               = recipient_obj
*        targetsystem                = targetsystem
*        targetserver                = targetserver
*        targetgroup                 = targetgroup
*        dont_release                = dont_release
*    ).
**      CATCH zcx_job.    "
*
*  ENDMETHOD.
*
*  METHOD zif_job_td~_job_submit.
*
*    spy = VALUE #( BASE spy
*        ( filter_out_initial_parameters( VALUE #(
*          func_name  = 'ABAP_SUBMIT'
*          parameters = VALUE #(
*            ( name = 'NAME' value = jobname ) ) ) ) ) ).
*
*    real->_job_submit(
*      EXPORTING
*        jobname                     = jobname
*        jobcount                    = jobcount
*        arcparams                   = arcparams
*        authcknam                   = authcknam
*        commandname                 = commandname
*        operatingsystem             = operatingsystem
*        extpgm_name                 = extpgm_name
*        extpgm_param                = extpgm_param
*        extpgm_set_trace_on         = extpgm_set_trace_on
*        extpgm_stderr_in_joblog     = extpgm_stderr_in_joblog
*        extpgm_stdout_in_joblog     = extpgm_stdout_in_joblog
*        extpgm_system               = extpgm_system
*        extpgm_rfcdest              = extpgm_rfcdest
*        extpgm_wait_for_termination = extpgm_wait_for_termination
*        language                    = language
*        priparams                   = priparams
*        report                      = report
*        variant                     = variant
*        this_routine                = this_routine
*      IMPORTING
*        step_number                 = step_number
*    ).
**      CATCH zcx_job.    " (
*
*  ENDMETHOD.
*
*  METHOD filter_out_initial_parameters.
*
*    spy_out = spy_in.
*    DELETE spy_out-parameters WHERE value IS INITIAL.
*
*  ENDMETHOD.
*
*  METHOD simple_immediately.
*
*    zcl_job=>inject_td( me ).
*
*    zcl_job=>new( name = 'one step'
*        )->add_step_abap( report = sy-repid
*        )->start_immediately( ).
*
*    cl_abap_unit_assert=>assert_equals(
*        act = spy
*        exp = VALUE ty_spy_lines(
*            ( func_name  = 'JOB_OPEN'
*              parameters = VALUE #(
*                ( name = 'NAME' value = 'one step' ) ) )
*            ( func_name  = 'ABAP_SUBMIT'
*              parameters = VALUE #(
*                ( name = 'NAME' value = 'one step' ) ) )
*            ( func_name  = 'JOB_CLOSE'
*              parameters = VALUE #(
*                ( name = 'NAME' value = 'one step' ) ) ) ) ).
*  ENDMETHOD.
*
*  METHOD two_steps_immediately.
*    zcl_job=>new( name = 'two steps'
*        )->add_step_abap( report = sy-repid
*        )->add_step_abap( report = sy-repid
*        )->start_immediately( ).
*  ENDMETHOD.
*
*  METHOD external_command_immediately.
*    zcl_job=>new( name = 'SM49'
*        )->add_step_external_command( command = 'ZDIR'
*        )->start_immediately( ).
*  ENDMETHOD.
*
*  METHOD external_program_immediately.
*    zcl_job=>new( name = 'OS command without SM49'
*        )->add_step_external_program( program = 'cmd /c dir' wait_for_termination = abap_true
*        )->start_immediately( ).
*  ENDMETHOD.
*
*  METHOD start_in_10_seconds.
*    zcl_job=>new( name = 'in 10 seconds'
*        )->add_step_abap( report = sy-repid
*        )->start_at( date = sy-datum time = CONV #( sy-uzeit + 10 ) ).
*  ENDMETHOD.
*
*  METHOD start_daily_at_10_am.
*    zcl_job=>new( name = 'daily at 10 AM'
*        )->add_step_abap( report = sy-repid
*        )->start_periodically( first_date = CONV #( sy-datum + 1 ) first_time = '100000' days = 1 ).
*  ENDMETHOD.
*
*  METHOD start_daily_except_holidays.
*    zcl_job=>new( name = 'daily except holidays'
*        )->start_periodically(
*            first_date = CONV #( sy-datum + 1 ) first_time = '100000'
*            days = 1
*            calendar_id = 'FR'
*            rule_if_date_falls_on_holiday = zcl_job=>calendar_rule-process_always ).
*  ENDMETHOD.
*
*  METHOD start_monthly_3rd_workday  .
*    zcl_job=>new( name = 'daily except holidays'
*        )->add_step_abap( report = sy-repid
*        )->start_monthly_nth_workday(
*            first_date = CONV #( sy-datum + 1 ) first_time = '100000'
*            months = 1
*            calendar_id = 'FR' nth_workday = 3 ).
*  ENDMETHOD.
*
*  METHOD serialize.
*
*    DATA(writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
*    CALL TRANSFORMATION id SOURCE data = data
*                            RESULT XML writer.
*    r_result = writer->get_output( ).
*
*  ENDMETHOD.
*
*ENDCLASS.
*
*PARAMETERS afterjob AS CHECKBOX DEFAULT 'X'.
*PARAMETERS immed AS CHECKBOX DEFAULT 'X'.
*PARAMETERS at_time AS CHECKBOX DEFAULT 'X'.
*
*START-OF-SELECTION.
*  DATA lt_sel TYPE rsparams_tt.
*  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
*    EXPORTING
*      curr_report     = sy-repid
*    TABLES
*      selection_table = lt_sel
*    EXCEPTIONS
*      not_found       = 1
*      no_report       = 2
*      OTHERS          = 3.
*  "BREAK-POINT.
*  CHECK sy-batch = abap_false.
*  NEW lcl_app( )->main( lt_sel ).
