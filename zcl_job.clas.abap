********************************************************************************
*  MIT License
*
*  Copyright (c) 2018 sandraros
*
*  Permission is hereby granted, free of charge, to any person obtaining a copy
*  of this software and associated documentation files (the "Software"), to deal
*  in the Software without restriction, including without limitation the rights
*  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*  copies of the Software, and to permit persons to whom the Software is
*  furnished to do so, subject to the following conditions:
*
*  The above copyright notice and this permission notice shall be included in all
*  copies or substantial portions of the Software.
*
*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*  SOFTWARE.
********************************************************************************
"! <p class="shorttext synchronized" lang="en">Background job</p>
CLASS zcl_job DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_job .

    CONSTANTS no_date TYPE d VALUE space ##NO_TEXT.
    CONSTANTS no_time TYPE t VALUE space ##NO_TEXT.

    "! (used instead of constructor to cast)
    CLASS-METHODS new
      IMPORTING
        !name           TYPE btcjob
        !class          TYPE bapixmjob-jobclass OPTIONAL "DEFAULT zif_job=>class-c
        !check_jobclass TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(job)      TYPE REF TO zif_job
      RAISING
        zcx_job .

    ALIASES:
        ty_ut_job                   FOR zif_job~ty_ut_job,
        ty_us_repeating_period      FOR zif_job~ty_us_repeating_period,
        ty_calendar_rule            FOR zif_job~ty_calendar_rule,
        ty_workday_count_direction  FOR zif_job~ty_workday_count_direction,
        ty_us_working_days          FOR zif_job~ty_us_working_days,
        direction                   FOR zif_job~direction,
        at_opmode                   FOR zif_job~at_opmode,
        at_opmode_periodic          FOR zif_job~at_opmode_periodic,
        calendar_id                 FOR zif_job~calendar_id,
        calendar_rule               FOR zif_job~calendar_rule,
        class                       FOR zif_job~class,
        count                       FOR zif_job~count,
        direct_start                FOR zif_job~direct_start ,
        dont_release                FOR zif_job~dont_release ,
        event_id                    FOR zif_job~event_id ,
        event_param                 FOR zif_job~event_param ,
        event_periodic              FOR zif_job~event_periodic ,
        jclass                      FOR zif_job~jclass ,
        laststrtdt                  FOR zif_job~laststrtdt ,
        laststrttm                  FOR zif_job~laststrttm ,
        name                        FOR zif_job~name ,
        prddays                     FOR zif_job~prddays ,
        prdhours                    FOR zif_job~prdhours ,
        prdmins                     FOR zif_job~prdmins ,
        prdmonths                   FOR zif_job~prdmonths ,
        prdweeks                    FOR zif_job~prdweeks ,
        predjob_checkstat           FOR zif_job~predjob_checkstat ,
        pred_jobcount               FOR zif_job~pred_jobcount ,
        pred_jobname                FOR zif_job~pred_jobname ,
        recipient_obj               FOR zif_job~recipient_obj ,
        sdlstrtdt                   FOR zif_job~sdlstrtdt ,
        sdlstrttm                   FOR zif_job~sdlstrttm ,
        startdate_restriction       FOR zif_job~startdate_restriction ,
        start_on_workday_not_before FOR zif_job~start_on_workday_not_before ,
        start_on_workday_nr         FOR zif_job~start_on_workday_nr ,
        state                       FOR zif_job~state ,
        strtimmed                   FOR zif_job~strtimmed ,
        targetgroup                 FOR zif_job~targetgroup ,
        targetserver                FOR zif_job~targetserver ,
        targetsystem                FOR zif_job~targetsystem ,
        workday_count_direction     FOR zif_job~workday_count_direction.

protected section.
  PRIVATE SECTION.

    CLASS-METHODS inject_td
      IMPORTING
        td TYPE REF TO zif_job_llop.

    CLASS-DATA default_td TYPE REF TO zif_job_llop.
    DATA td TYPE REF TO zif_job_llop.

    METHODS constructor
      IMPORTING
        !name           TYPE btcjob
        !jobgroup       TYPE btcjobgrp OPTIONAL
        !class          TYPE bapixmjob-jobclass DEFAULT zif_job=>class-c
        !check_jobclass TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_job .

    CLASS-METHODS convert_sy_to_bapiret2
      RETURNING
        VALUE(bapiret2) TYPE bapiret2 .

    CLASS-METHODS check_ret_code
      IMPORTING
        ret          TYPE i
        this_routine TYPE csequence
      RAISING
        zcx_job .

    METHODS _close
      RAISING
        zcx_job .


    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter print_parameters | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter archive_parameters | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter report | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter user | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ARCPARAMS | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter PRIPARAMS | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_job | <p class="shorttext synchronized" lang="en"></p>
    METHODS process_print_archive_params
      IMPORTING
        VALUE(print_parameters)   TYPE bapipripar
        VALUE(archive_parameters) TYPE bapiarcpar
        report                    TYPE program
        user                      TYPE syuname
      EXPORTING
        arcparams                 TYPE arc_params
        priparams                 TYPE pri_params
      RAISING
        zcx_job .

ENDCLASS.



CLASS ZCL_JOB IMPLEMENTATION.


  METHOD check_ret_code.
    DATA dummy TYPE string.

    " based on subroutine XM_MAKE_BAPIRET2_EX in program SAPLSXBP
    CASE ret.
      WHEN tybtc_err_invalid_step_number.
        IF 0 = 1. MESSAGE e224(xm). ENDIF. " Wrong step number
        MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_wrong_step_number INTO dummy.
      WHEN tybtc_err_no_authority.
        IF 0 = 1. MESSAGE e234(xm). ENDIF. " You do not have change authorization
        MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_no_change_authority INTO dummy.
      WHEN tybtc_err_job_doesnt_have_step.
        IF 0 = 1. MESSAGE e220(xm). ENDIF. " Step not in job
        MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_job_doesnt_have_this_step INTO dummy.
      WHEN tybtc_err_child_register_error.
        IF 0 = 1. MESSAGE e089(xm). ENDIF. " Error while registering a child job
        MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_child_register_error INTO dummy.
      WHEN tybtc_err_wrong_selection_par.
        IF 0 = 1. MESSAGE e096(xm). ENDIF. " Wrong selection parameters
        MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_wrong_selection_par INTO dummy.
      WHEN tybtc_err_invalid_jobclass.
        IF 0 = 1. MESSAGE e235(xm). ENDIF. " Invalid job class
        MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_invalid_jobclass INTO dummy.
      WHEN tybtc_err_spoollist_recipient.
        IF 0 = 1. MESSAGE e237(xm). ENDIF. " Receiver object could not be created
        MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_cant_create_rec_object INTO dummy.
      WHEN tybtc_err_plain_recipient.
        IF 0 = 1. MESSAGE e270(xm). ENDIF. " Could not determine recipient data
        MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_cant_return_recipient INTO dummy.
      WHEN OTHERS.
        IF 0 = 1. MESSAGE e034(xm). ENDIF. " Internal problem (function &1)
        MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_problem_detected WITH this_routine INTO dummy.
    ENDCASE.
  ENDMETHOD.


  METHOD constructor.

    me->name   = name.
    me->jclass = class.
    IF default_td IS NOT BOUND.
      me->td   = NEW lcl_low_level_ops( ).
    ELSE.
      me->td   = default_td.
    ENDIF.

  ENDMETHOD.


  METHOD convert_sy_to_bapiret2.

    bapiret2-id = sy-msgid.
    bapiret2-type = sy-msgty.
    bapiret2-number = sy-msgno.
    bapiret2-message_v1 = sy-msgv1.
    bapiret2-message_v2 = sy-msgv2.
    bapiret2-message_v3 = sy-msgv3.
    bapiret2-message_v4 = sy-msgv4.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO bapiret2-message.

  ENDMETHOD.


  METHOD inject_td.

    zcl_job=>default_td = td.

  ENDMETHOD.


  METHOD new.
    DATA: info TYPE i,
          ret  TYPE i.

    DATA(new_job) = NEW zcl_job(
        name  = name
        class = class ).

    new_job->sdlstrtdt = no_date. " date with spaces instead of zeroes, as defined in JOB_CLOSE
    new_job->sdlstrttm = no_time. " time with spaces instead of zeroes, as defined in JOB_CLOSE

    new_job->td->job_open(
      EXPORTING
        jobname          = new_job->name
        jobclass         = new_job->jclass
        check_jobclass   = check_jobclass
      IMPORTING
        jobcount         = new_job->count
        info             = info
      CHANGING
        ret              = ret ).

    job = new_job.

  ENDMETHOD.


  METHOD process_print_archive_params.
    CONSTANTS : this_routine         TYPE symsgv VALUE 'ADD_STEP_ABAP' ##NO_TEXT,
                c_char_unknown       TYPE c VALUE '_', "Unbekannt C
                c_int_unknown        TYPE i VALUE -1,  "Unbekannt I
                c_num1_unknown       TYPE n VALUE '0', "Unbekannt N(1)
                c_char_space_request TYPE c VALUE '$'. "will be SPACE
    DATA dummy TYPE string.

    " code taken from subroutine INIT_PRINT_PARAMETERS in program SAPLSXBP.

    " for some values, we have to rely on the caller for
    " the correct initialization because SPACE is not the
    " default value

    IF print_parameters-primm IS INITIAL OR
       print_parameters-primm = space.
      print_parameters-primm = c_char_unknown.
      " caller must pass '$' if 'do not print immediately' shall be set
    ELSEIF print_parameters-primm = c_char_space_request.
      CLEAR print_parameters-primm.
    ENDIF.

    IF print_parameters-prrel IS INITIAL.
      print_parameters-prrel = c_char_unknown.
      " caller must pass '$' if 'do not release after print' shall be set
    ELSEIF print_parameters-prrel = c_char_space_request.
      CLEAR print_parameters-prrel.
    ENDIF.

    IF print_parameters-prnew IS INITIAL.
      print_parameters-prnew = c_char_unknown.
      " caller must pass '$' if 'append spool' shall be set
    ELSEIF print_parameters-prnew = c_char_space_request.
      CLEAR print_parameters-prnew.
    ENDIF.

    IF print_parameters-prsap IS INITIAL.
      print_parameters-prsap = c_char_unknown.
      " caller must pass '$' if 'no SAP cover page' shall be set
    ELSEIF print_parameters-prsap = c_char_space_request.
      CLEAR print_parameters-prsap.
    ENDIF.

    IF print_parameters-prunx IS INITIAL.
      print_parameters-prunx = c_char_unknown.
      " caller must pass '$' if 'no host spool cover page' shall be set
    ELSEIF print_parameters-prunx = c_char_space_request.
      CLEAR print_parameters-prunx.
    ENDIF.

    IF print_parameters-prcop IS INITIAL.
      print_parameters-prcop = '1'.                              " 1 Kopie
    ENDIF.

    IF print_parameters-armod IS INITIAL.
      print_parameters-armod = '1'. " Drucken
    ENDIF.

    IF print_parameters-prrec IS INITIAL.
      print_parameters-prrec = user.
      " caller must pass '$' if SPACE shall be set
    ELSEIF print_parameters-prrec = c_char_space_request.
      CLEAR print_parameters-prrec.
    ENDIF.

    IF print_parameters-linct IS INITIAL.
      print_parameters-linct = c_int_unknown.
    ENDIF.

    IF print_parameters-linsz IS INITIAL.
      print_parameters-linsz = c_int_unknown.
    ENDIF.

    IF print_parameters-pdest IS INITIAL.
      print_parameters-pdest = c_char_unknown.
    ENDIF.

    IF print_parameters-plist IS INITIAL.
      print_parameters-plist = c_char_unknown.
    ENDIF.

    IF print_parameters-prtxt IS INITIAL.
      print_parameters-prtxt = c_char_unknown.
    ENDIF.

    IF print_parameters-pexpi IS INITIAL.
      print_parameters-pexpi = c_num1_unknown.
    ENDIF.

    IF print_parameters-paart IS INITIAL.
      print_parameters-paart = c_char_unknown.
    ENDIF.

    IF print_parameters-prbig IS INITIAL.
      print_parameters-prbig = c_char_unknown.
    ENDIF.

    IF print_parameters-prabt IS INITIAL.
      print_parameters-prabt = c_char_unknown.
    ENDIF.

    IF print_parameters-prabt = 'SPACE'.
      CLEAR print_parameters-prabt.
      " caller must pass '$' if SPACE shall be set
    ELSEIF print_parameters-prabt = c_char_space_request.
      CLEAR print_parameters-prabt.
    ENDIF.

    IF print_parameters-prber IS INITIAL.
      print_parameters-prber = c_char_unknown.
    ENDIF.

    IF print_parameters-prdsn IS INITIAL.
      print_parameters-prdsn = c_char_unknown.
    ENDIF.

    print_parameters-ptype = c_char_unknown.

    IF print_parameters-footl IS INITIAL.
      print_parameters-footl = c_char_unknown.
    ENDIF.

    IF print_parameters-priot IS INITIAL.
      print_parameters-priot = c_num1_unknown.
    ENDIF.

    IF archive_parameters-archiv_id IS INITIAL.
      archive_parameters-archiv_id = c_char_unknown.
    ENDIF.

    IF archive_parameters-info IS INITIAL.
      archive_parameters-info = c_char_unknown.
    ENDIF.

    IF archive_parameters-arctext IS INITIAL.
      archive_parameters-arctext = c_char_unknown.
    ENDIF.

    IF archive_parameters-ar_object IS INITIAL.
      archive_parameters-ar_object = c_char_unknown.
    ENDIF.

    IF archive_parameters-report IS INITIAL.
      archive_parameters-report = c_char_unknown.
    ENDIF.

    IF archive_parameters-sap_object IS INITIAL.
      archive_parameters-sap_object = c_char_unknown.
    ENDIF.

    MOVE-CORRESPONDING archive_parameters TO arcparams.
    MOVE-CORRESPONDING print_parameters TO priparams.

    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
        archive_id             = arcparams-archiv_id
        archive_info           = arcparams-info
        archive_mode           = priparams-armod
        archive_text           = arcparams-arctext
        ar_object              = arcparams-ar_object
        archive_report         = arcparams-report
        authority              = priparams-prber
        copies                 = priparams-prcop
        cover_page             = priparams-prbig
        data_set               = priparams-prdsn
        department             = priparams-prabt
        destination            = priparams-pdest
        expiration             = priparams-pexpi
        immediately            = priparams-primm
        layout                 = priparams-paart
        line_count             = priparams-linct
        line_size              = priparams-linsz
        list_name              = priparams-plist
        list_text              = priparams-prtxt
        mode                   = 'BATCH' ##NO_TEXT
        new_list_id            = priparams-prnew
        no_dialog              = 'X'
        receiver               = priparams-prrec
        release                = priparams-prrel
        report                 = report
        sap_cover_page         = priparams-prsap
        host_cover_page        = priparams-prunx
        priority               = priparams-priot
        sap_object             = arcparams-sap_object
        type                   = priparams-ptype
        user                   = user
      IMPORTING
        out_archive_parameters = arcparams
        out_parameters         = priparams
      EXCEPTIONS
        archive_info_not_found = 1
        invalid_print_params   = 2
        invalid_archive_params = 3
        OTHERS                 = 4.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          IF 0 = 1. MESSAGE e051(xm). ENDIF. " No archive information found (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_no_archive_info WITH this_routine.
        WHEN 2.
          IF 0 = 1. MESSAGE e052(xm). ENDIF. " Invalid print information (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_invalid_print_params WITH this_routine.
        WHEN 3.
          IF 0 = 1. MESSAGE e053(xm). ENDIF. " Invalid archive information (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_invalid_archive_params WITH this_routine.
        WHEN 4.
          IF 0 = 1. MESSAGE e034(xm). ENDIF. " Internal problem (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_problem_detected WITH this_routine.
      ENDCASE.

      zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_job~add_step_abap.

    CONSTANTS : this_routine TYPE symsgv VALUE 'ADD_STEP_ABAP' ##NO_TEXT.
    DATA: arcparams   TYPE arc_params,
          priparams   TYPE pri_params,
          step_number TYPE i.


*    process_print_archive_params(
*      EXPORTING
*        archive_parameters = archive_parameters
*        print_parameters   = print_parameters
*        report             = report
*        user               = user
*      IMPORTING
*        arcparams          = arcparams
*        priparams          = priparams ).

    IF selection_table IS INITIAL AND free_selections IS INITIAL.

      td->job_submit(
        EXPORTING
          jobname     = me->name
          jobcount    = me->count
          arcparams   = arcparams
          authcknam   = user
          language    = language
          priparams   = priparams
          report      = report
          variant     = variant
          this_routine = this_routine
        IMPORTING
          step_number = step_number ).

    ELSE.

      td->abap_submit(
          jobname         = me->name
          jobcount        = me->count
          selection_table = selection_table
          arcparams       = arcparams
          user            = user
          priparams       = priparams
          report          = report
          variant         = variant
          this_routine    = this_routine ).

    ENDIF.

    job = me.

  ENDMETHOD.


  METHOD zif_job~add_step_external_command.
    CONSTANTS : this_routine TYPE symsgv VALUE 'ADD_STEP_EXTERNAL_COMMAND' ##NO_TEXT.
    DATA: step_number TYPE i.

    td->job_submit(
      EXPORTING
        jobname                     = me->name
        jobcount                    = me->count
        commandname                 = command
        extpgm_param                = parameters
        operatingsystem             = operating_system
        extpgm_rfcdest              = rfcdest
        extpgm_set_trace_on         = set_trace_on
        extpgm_stderr_in_joblog     = stderr_in_joblog
        extpgm_stdout_in_joblog     = stdout_in_joblog
        extpgm_wait_for_termination = wait_for_termination
        authcknam                   = user
        this_routine                = this_routine
      IMPORTING
        step_number                 = step_number ).

    job = me.

  ENDMETHOD.


  METHOD zif_job~add_step_external_program.
    CONSTANTS : this_routine TYPE symsgv VALUE 'ADD_STEP_EXTERNAL_PROGRAM' ##NO_TEXT.
    DATA: step_number TYPE i.

    td->job_submit(
      EXPORTING
        jobname                     = me->name
        jobcount                    = me->count
        extpgm_name                 = program
        extpgm_param                = parameters
        extpgm_system               = server
        extpgm_rfcdest              = rfcdest
        extpgm_set_trace_on         = set_trace_on
        extpgm_stderr_in_joblog     = stderr_in_joblog
        extpgm_stdout_in_joblog     = stdout_in_joblog
        extpgm_wait_for_termination = wait_for_termination
        authcknam                   = user
        this_routine                = this_routine
      IMPORTING
        step_number                 = step_number ).

    job = me.

  ENDMETHOD.


  METHOD zif_job~get_state.
    DATA: job_read_jobhead TYPE tbtcjob,
          dummy            TYPE string.

    IF check_actual_status = abap_false.

      CALL FUNCTION 'BP_JOB_READ'
        EXPORTING
          job_read_jobcount     = count
          job_read_jobname      = name
          job_read_opcode       = tybtc_read_jobhead_only
        IMPORTING
          job_read_jobhead      = job_read_jobhead
        EXCEPTIONS
          invalid_opcode        = 1 " TODO impossible
          job_doesnt_exist      = 2
          job_doesnt_have_steps = 3
          OTHERS                = 99.

      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 2.
            IF 0 = 1. MESSAGE e049(xm). ENDIF. " Job does not exist (function &1)
            MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_job_does_not_exist WITH 'BP_JOB_READ' INTO dummy ##NO_TEXT.
          WHEN OTHERS.
            " TODO msg_problem_detected
        ENDCASE.

        zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).

      ENDIF.

      state = job_read_jobhead-status.

    ELSE.

      CALL FUNCTION 'BP_JOB_CHECKSTATE'
        EXPORTING
          dialog                       = 'N'
          jobcount                     = count
          jobname                      = name
          time_limit                   = 60
        IMPORTING
          actual_status                = state
        EXCEPTIONS
          checking_of_job_has_failed   = 1
          correcting_job_status_failed = 2
          "invalid_dialog_type          = 3 " TODO impossible
          job_does_not_exist           = 4
          no_check_privilege_given     = 5
          ready_switch_too_dangerous   = 0 "normal situation below 60 seconds
          OTHERS                       = 7.

      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 4.
            IF 0 = 1. MESSAGE e049(xm). ENDIF. " Job does not exist (function &1)
            MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_job_does_not_exist WITH 'BP_JOB_CHECKSTATE' INTO dummy ##NO_TEXT.
          WHEN 5.
            IF 0 = 1. MESSAGE e064(xm). ENDIF. " No authorization to execute the operation
            MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_privilege_missing INTO dummy.
          WHEN OTHERS.
            IF 0 = 1. MESSAGE e064(xm). ENDIF. " No authorization to execute the operation
            MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_problem_detected INTO dummy.
        ENDCASE.

        zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_job~set_server.

    me->targetserver = server.
    CLEAR : me->targetgroup, me->targetsystem.
    job = me.

  ENDMETHOD.


  METHOD zif_job~set_server_group.

    me->targetgroup = server_group.
    CLEAR : me->targetserver, me->targetsystem.
    job = me.

  ENDMETHOD.


  METHOD zif_job~set_server_old.

    me->targetsystem = server_old.
    CLEAR : me->targetserver, me->targetgroup.
    job = me.

  ENDMETHOD.


  METHOD zif_job~start_after_job.

    pred_jobcount = predecessor->count.
    pred_jobname = predecessor->name.
    _close( ).

  ENDMETHOD.


  METHOD zif_job~start_at.

    me->sdlstrtdt = date.
    me->sdlstrttm = time.
    _close( ).

  ENDMETHOD.


  METHOD zif_job~start_at_event.

    me->event_id = event_id.
    me->event_param = event_param.
    me->event_periodic = event_periodic.
    _close( ).

  ENDMETHOD.


  METHOD zif_job~start_at_opmode_switch.

    me->at_opmode = opmode.
    me->at_opmode_periodic = opmode_periodic.
    _close( ).

  ENDMETHOD.


  METHOD zif_job~start_immediately.

    me->strtimmed = abap_true.
    IF error_if_cant_start_immed = abap_true.
      me->direct_start = abap_true.
    ELSE.
      me->direct_start = abap_false.
    ENDIF.
    _close( ).

  ENDMETHOD.


  METHOD zif_job~start_monthly_nth_workday.
    DATA: tstmp      TYPE timestamp,
          tstmp_numc TYPE n LENGTH 14.

    me->sdlstrtdt = first_date.
    me->sdlstrttm = first_time.
    tstmp = first_date && first_time.
    tstmp_numc = tstmp = round( val = cl_abap_tstmp=>add( tstmp = tstmp secs = skip_if_not_started_in_minutes * 60 ) dec = 0 ).
    me->laststrtdt = tstmp_numc(8).
    me->laststrttm = tstmp_numc+8(6).
    me->prdmonths = months.
    me->calendar_id = calendar_id.
    IF nth_workday > 0.
      me->start_on_workday_nr = nth_workday.
      me->workday_count_direction = tybtc_beginning_of_month.
    ELSE.
      me->start_on_workday_nr = -1 * nth_workday.
      me->workday_count_direction = tybtc_end_of_month.
    ENDIF.
    _close( ).

  ENDMETHOD.


  METHOD zif_job~start_periodically.
    DATA: tstmp      TYPE timestamp,
          tstmp_numc TYPE n LENGTH 14.

    me->sdlstrtdt = first_date.
    me->sdlstrttm = first_time.
    tstmp = first_date && first_time.
    tstmp_numc = tstmp = round( val = cl_abap_tstmp=>add( tstmp = tstmp secs = skip_if_not_started_in_minutes * 60 ) dec = 0 ).
    me->laststrtdt = tstmp_numc(8).
    me->laststrttm = tstmp_numc+8(6).
    me->prddays   = days  .
    me->prdhours  = hours .
    me->prdmins   = mins  .
    me->prdmonths = months.
    me->prdweeks  = weeks .
    me->startdate_restriction = rule_if_date_falls_on_holiday.
    me->calendar_id = calendar_id.
    _close( ).

  ENDMETHOD.


  METHOD _close.

    td->job_close(
          EXPORTING
            jobcount                    = me->count
            jobname                     = me->name
            "--------------- mode ----------------
            at_opmode                   = at_opmode
            at_opmode_periodic          = at_opmode_periodic
            "--------------- event ---------------
            event_id                    = event_id
            event_param                 = event_param
            event_periodic              = event_periodic
            "----------- periodically -------------
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
            "------------- predecessor job --------------
            predjob_checkstat           = predjob_checkstat
            pred_jobcount               = pred_jobcount
            pred_jobname                = pred_jobname
            "------------------------------------------
            strtimmed                   = strtimmed
            direct_start                = direct_start
            "------------------------------------------
            recipient_obj               = recipient_obj
            "------------------------------------------
            targetsystem                = targetsystem
            targetserver                = targetserver
            targetgroup                 = targetgroup
            "------------------------------------------
            dont_release                = dont_release ).

  ENDMETHOD.
ENDCLASS.
