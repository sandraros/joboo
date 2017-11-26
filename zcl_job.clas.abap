CLASS zcl_job DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_ut_job TYPE TABLE OF REF TO zcl_job .

    CONSTANTS no_date TYPE d VALUE space.                   "#EC NOTEXT
    CONSTANTS no_time TYPE t VALUE space.                   "#EC NOTEXT
    DATA name TYPE btcjob .
    DATA count TYPE btcjobcnt .
    DATA state TYPE tbtco-status .

    METHODS constructor
      IMPORTING
        !iv_name TYPE btcjob
      RAISING
        zcx_job_open .
    METHODS start_immediately
      RAISING
        zcx_job_close .
    METHODS add_step_abap
      IMPORTING
        !report TYPE program
        !variant TYPE variant
        !user TYPE syuname DEFAULT sy-uname
        !selection_table TYPE rsparams_tt OPTIONAL
      RAISING
        zcx_job_step .
    METHODS schedule_at
      IMPORTING
        !date TYPE d
        !time TYPE t
      RAISING
        zcx_job_close .
    METHODS schedule_after_job
      IMPORTING
        !io_job TYPE REF TO zcl_job
      RAISING
        zcx_job_close .
    CLASS-METHODS wait_jobs
      IMPORTING
        !it_job TYPE ty_ut_job OPTIONAL
        !iv_delay TYPE numeric DEFAULT 10
      PREFERRED PARAMETER it_job .
protected section.
PRIVATE SECTION.

  DATA at_opmode TYPE spfba-baname .
  DATA at_opmode_periodic TYPE btch0000-char1 .
  DATA calendar_id TYPE tbtcjob-calendarid .
  DATA event_id TYPE tbtcjob-eventid .
  DATA event_param TYPE tbtcjob-eventparm .
  DATA event_periodic TYPE btch0000-char1 .
  DATA laststrtdt TYPE tbtcjob-laststrtdt .
  DATA laststrttm TYPE tbtcjob-laststrttm .
  DATA prddays TYPE tbtcjob-prddays .
  DATA prdhours TYPE tbtcjob-prdhours .
  DATA prdmins TYPE tbtcjob-prdmins .
  DATA prdmonths TYPE tbtcjob-prdmonths .
  DATA prdweeks TYPE tbtcjob-prdweeks .
  DATA predjob_checkstat TYPE tbtcstrt-checkstat .
  DATA pred_jobcount TYPE tbtcjob-jobcount .
  DATA pred_jobname TYPE tbtcjob-jobname .
  DATA sdlstrtdt TYPE tbtcjob-sdlstrtdt .
  DATA sdlstrttm TYPE tbtcjob-sdlstrttm .
  DATA startdate_restriction TYPE tbtcjob-prdbehav .
  DATA strtimmed TYPE btch0000-char1 .
  DATA targetsystem TYPE msxxlist-name .
  DATA start_on_workday_not_before TYPE tbtcstrt-notbefore .
  DATA start_on_workday_nr TYPE tbtcstrt-wdayno .
  DATA workday_count_direction TYPE tbtcstrt-wdaycdir .
  DATA recipient_obj TYPE swotobjid .
  DATA targetserver TYPE btctgtsrvr-srvname .
  DATA dont_release TYPE btch0000-char1 .
  DATA targetgroup TYPE bpsrvgrp .
  DATA direct_start TYPE btch0000-char1 .

  METHODS close
    RAISING
      zcx_job_close .
ENDCLASS.



CLASS ZCL_JOB IMPLEMENTATION.


METHOD add_step_abap.

  IF selection_table IS INITIAL.
    CALL FUNCTION 'JOB_SUBMIT'
      EXPORTING
*         arcparams               =
        authcknam               = user
*         commandname             = SPACE
*         operatingsystem         = SPACE
*         extpgm_name             = SPACE
*         extpgm_param            = SPACE
*         extpgm_set_trace_on     = SPACE
*         extpgm_stderr_in_joblog = 'X'
*         extpgm_stdout_in_joblog = 'X'
*         extpgm_system           = SPACE
*         extpgm_rfcdest          = SPACE
*         extpgm_wait_for_termination = 'X'
        jobcount                = me->count
        jobname                 = me->name
*         language                = SY-LANGU
*         priparams               = SPACE
        report                  = report
        variant                 = variant
*        IMPORTING
*         step_number             =
      EXCEPTIONS
        bad_priparams           = 1
        bad_xpgflags            = 2
        invalid_jobdata         = 3
        jobname_missing         = 4
        job_notex               = 5
        job_submit_failed       = 6
        lock_failed             = 7
        program_missing         = 8
        prog_abap_and_extpg_set = 9
        OTHERS                  = 10.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_step.
    ENDIF.
  ELSEIF variant IS INITIAL AND user IS INITIAL.
    SUBMIT (report)
          VIA JOB me->name NUMBER me->count
          WITH SELECTION-TABLE selection_table
          AND RETURN.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_step.
    ENDIF.
  ELSEIF variant IS INITIAL.
    SUBMIT (report)
          VIA JOB me->name NUMBER me->count
          USER user
          WITH SELECTION-TABLE selection_table
          AND RETURN.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_step.
    ENDIF.
  ELSEIF user IS INITIAL.
    SUBMIT (report)
          VIA JOB me->name NUMBER me->count
          USING SELECTION-SET variant
          WITH SELECTION-TABLE selection_table
          AND RETURN.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_step.
    ENDIF.
  ELSE.
    SUBMIT (report)
          VIA JOB me->name NUMBER me->count
          USER user
          USING SELECTION-SET variant
          WITH SELECTION-TABLE selection_table
          AND RETURN.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_step.
    ENDIF.
  ENDIF.

ENDMETHOD.


method CLOSE.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount                    = me->count
        jobname                     = me->name
        " --------------- mode ----------------
        at_opmode                   = at_opmode
        at_opmode_periodic          = at_opmode_periodic
        calendar_id                 = calendar_id
        " ------------ événement --------------
        event_id                    = event_id
        event_param                 = event_param
        event_periodic              = event_periodic
        laststrtdt                  = laststrtdt
        laststrttm                  = laststrttm
        " ----------- périodique --------------
        prddays                     = prddays
        prdhours                    = prdhours
        prdmins                     = prdmins
        prdmonths                   = prdmonths
        prdweeks                    = prdweeks
        " ---------- job prédécesseur ---------
        predjob_checkstat           = predjob_checkstat
        pred_jobcount               = pred_jobcount
        pred_jobname                = pred_jobname
        " -------------------------------------
        sdlstrtdt                   = sdlstrtdt
        sdlstrttm                   = sdlstrttm
        startdate_restriction       = startdate_restriction
        strtimmed                   = strtimmed
        start_on_workday_not_before = start_on_workday_not_before
        start_on_workday_nr         = start_on_workday_nr
        workday_count_direction     = workday_count_direction
        recipient_obj               = recipient_obj
        targetsystem                = targetsystem
        targetserver                = targetserver
        targetgroup                 = targetgroup
        dont_release                = dont_release
        direct_start                = direct_start
*        IMPORTING
*       job_was_released            =
*        CHANGING
*       ret                         =
      EXCEPTIONS
        cant_start_immediate        = 1
        invalid_startdate           = 2
        jobname_missing             = 3
        job_close_failed            = 4
        job_nosteps                 = 5
        job_notex                   = 6
        lock_failed                 = 7
        invalid_target              = 8
        OTHERS                      = 9.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_job_close.
*        MESSAGE 'Error while closing a job'(002) TYPE 'X'.
    ENDIF.

endmethod.


method CONSTRUCTOR.

    me->name = iv_name.
    sdlstrtdt = no_date.
    sdlstrttm = no_time.
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = me->name
      IMPORTING
        jobcount         = me->count
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_job_open.
*      MESSAGE 'Error while opening a job'(001) TYPE 'X'.
    ENDIF.

endmethod.


method SCHEDULE_AFTER_JOB.

    pred_jobcount = io_job->count.
    pred_jobname = io_job->name.
    close( ).

endmethod.


method SCHEDULE_AT.

    sdlstrtdt = date.
    sdlstrttm = time.
    close( ).

endmethod.


method START_IMMEDIATELY.

    strtimmed = abap_true.
    close( ).

endmethod.


METHOD wait_jobs.

  DATA lo_job TYPE REF TO zcl_job.

  DO.

    LOOP AT it_job INTO lo_job
          WHERE table_line IS BOUND
            AND table_line->state <> tybtc_finished
            AND table_line->state <> tybtc_aborted.

      CALL FUNCTION 'BP_JOB_CHECKSTATE'
        EXPORTING
          dialog                       = 'N'
          jobcount                     = lo_job->count
          jobname                      = lo_job->name
          time_limit                   = 60
        IMPORTING
          actual_status                = lo_job->state
        EXCEPTIONS
          checking_of_job_has_failed   = 1
          correcting_job_status_failed = 2
          invalid_dialog_type          = 3
          job_does_not_exist           = 4
          no_check_privilege_given     = 5
          ready_switch_too_dangerous   = 0 "normal situation below 60 seconds
          OTHERS                       = 7.
      IF sy-subrc <> 0.
        MESSAGE 'Error while testing the job status'(003) TYPE 'X'.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF sy-subrc <> 0.
      " all jobs are finished or aborted.
      RETURN.
    ENDIF.

    WAIT UP TO iv_delay SECONDS.

  ENDDO.


ENDMETHOD.
ENDCLASS.
