"! Start methods:
"! <ul>
"! <li>START_IMMEDIATELY</li>
"! <li>START_AT (given date and time)</li>
"! <li>START_PERIODICALLY</li>
"! <li>START_MONTHLY_NTH_WORKDAY</li>
"! <li>START_AFTER_JOB</li>
"! <li>START_AT_EVENT</li>
"! <li>START_AT_OPMODE_SWITCH</li>
"! <li></li>
"! </ul>
"! <p class="shorttext synchronized" lang="en">Background Job interface</p>
INTERFACE zif_job
  PUBLIC .


  TYPES:
    ty_ut_job TYPE TABLE OF REF TO zif_job .
  TYPES:
    BEGIN OF ty_us_repeating_period,
      weeks TYPE tbtcjob-prdweeks,
      days  TYPE tbtcjob-prddays,
      hours TYPE tbtcjob-prdhours,
      mins  TYPE tbtcjob-prdmins,
    END OF ty_us_repeating_period .
  TYPES:
    ty_calendar_rule TYPE c LENGTH 1 .
  TYPES ty_workday_count_direction TYPE tbtcstrt-wdaycdir .
  TYPES:
    BEGIN OF ty_us_working_days,
      rule                    TYPE ty_calendar_rule,
      calendar_id             TYPE tbtcjob-calendarid,
      start_on_workday_nr     TYPE tbtcstrt-wdayno,
      workday_count_direction TYPE tbtcstrt-wdaycdir,
    END OF ty_us_working_days .

  CONSTANTS:
    BEGIN OF class,
      "! A = highest priority
      a TYPE btcjobclas VALUE tybtc_jobclass_a,
      "! B = medium priority (default)
      b TYPE btcjobclas VALUE tybtc_jobclass_b,
      "! C = low priority
      c TYPE btcjobclas VALUE tybtc_jobclass_c,
    END OF class .

  CONSTANTS:
    BEGIN OF state,
      running       TYPE btcstatus VALUE tybtc_running,
      ready         TYPE btcstatus VALUE tybtc_ready,
      scheduled     TYPE btcstatus VALUE tybtc_scheduled,
      intercepted   TYPE btcstatus VALUE tybtc_intercepted,
      released      TYPE btcstatus VALUE tybtc_released,
      aborted       TYPE btcstatus VALUE tybtc_aborted,
      finished      TYPE btcstatus VALUE tybtc_finished,
      put_active    TYPE btcstatus VALUE tybtc_put_active,
      unknown_state TYPE btcstatus VALUE tybtc_unknown_state,
    END OF state .
  CONSTANTS:
    BEGIN OF direction,
      btc_beginning_of_month TYPE ty_workday_count_direction VALUE tybtc_beginning_of_month,
      btc_end_of_month       TYPE ty_workday_count_direction VALUE tybtc_end_of_month,
    END OF direction .
  CONSTANTS:
    BEGIN OF calendar_rule,
      dont_process_on_holiday TYPE ty_calendar_rule VALUE tybtc_dont_process_on_holiday,
      process_before_holiday  TYPE ty_calendar_rule VALUE tybtc_process_before_holiday,
      process_after_holiday   TYPE ty_calendar_rule VALUE tybtc_process_after_holiday,
      process_always          TYPE ty_calendar_rule VALUE tybtc_process_always,
    END OF calendar_rule .
  DATA: name                        TYPE btcjob READ-ONLY,
        count                       TYPE btcjobcnt READ-ONLY,
        jclass                      TYPE btcjobclas READ-ONLY,
        at_opmode                   TYPE spfba-baname READ-ONLY,
        at_opmode_periodic          TYPE btch0000-char1 READ-ONLY,
        event_id                    TYPE tbtcjob-eventid READ-ONLY,
        event_param                 TYPE tbtcjob-eventparm READ-ONLY,
        event_periodic              TYPE btch0000-char1 READ-ONLY,
        sdlstrtdt                   TYPE tbtcjob-sdlstrtdt READ-ONLY,
        sdlstrttm                   TYPE tbtcjob-sdlstrttm READ-ONLY,
        laststrtdt                  TYPE tbtcjob-laststrtdt READ-ONLY,
        laststrttm                  TYPE tbtcjob-laststrttm READ-ONLY,
        prddays                     TYPE tbtcjob-prddays READ-ONLY,
        prdhours                    TYPE tbtcjob-prdhours READ-ONLY,
        prdmins                     TYPE tbtcjob-prdmins READ-ONLY,
        prdmonths                   TYPE tbtcjob-prdmonths READ-ONLY,
        prdweeks                    TYPE tbtcjob-prdweeks READ-ONLY,
        calendar_id                 TYPE tbtcjob-calendarid READ-ONLY,
        startdate_restriction       TYPE tbtcjob-prdbehav READ-ONLY,
        start_on_workday_not_before TYPE tbtcstrt-notbefore READ-ONLY,
        start_on_workday_nr         TYPE tbtcstrt-wdayno READ-ONLY,
        workday_count_direction     TYPE tbtcstrt-wdaycdir READ-ONLY,
        predjob_checkstat           TYPE tbtcstrt-checkstat READ-ONLY,
        pred_jobcount               TYPE tbtcjob-jobcount READ-ONLY,
        pred_jobname                TYPE tbtcjob-jobname READ-ONLY,
        strtimmed                   TYPE btch0000-char1 READ-ONLY,
        direct_start                TYPE btch0000-char1 READ-ONLY,
        targetsystem                TYPE msxxlist-name READ-ONLY,
        targetserver                TYPE btctgtsrvr-srvname READ-ONLY,
        targetgroup                 TYPE bpsrvgrp READ-ONLY,
        recipient_obj               TYPE swotobjid READ-ONLY,
        dont_release                TYPE btch0000-char1 READ-ONLY.

  "! <p class="shorttext synchronized" lang="en">OBSOLETE - use SET_SERVER</p>
  METHODS set_server_old
    IMPORTING
      !server_old TYPE msxxlist-name
    RETURNING
      VALUE(job)  TYPE REF TO zif_job.

  "! <p class="shorttext synchronized" lang="en">OBSOLETE - use SET_SERVER</p>
  METHODS set_server
    IMPORTING
      !server    TYPE btctgtsrvr-srvname
    RETURNING
      VALUE(job) TYPE REF TO zif_job .

  "! <p class="shorttext synchronized" lang="en">Start on specific group of application servers (SM61)</p>
  METHODS set_server_group
    IMPORTING
      !server_group TYPE bpsrvgrp
    RETURNING
      VALUE(job)    TYPE REF TO zif_job .
  TYPE-POOLS abap .

  "! <p class="shorttext synchronized" lang="en">Start job immediately</p>
  METHODS start_immediately
    IMPORTING
      !error_if_cant_start_immed TYPE abap_bool DEFAULT abap_false
    RAISING
      zcx_job .

  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter report | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter variant | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter user | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter language | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter free_selections | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter selection_table | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter print_parameters | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter archive_parameters | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter job | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_job | <p class="shorttext synchronized" lang="en"></p>
  METHODS add_step_abap
    IMPORTING
      !report             TYPE program
      !variant            TYPE variant OPTIONAL
      !user               TYPE syuname DEFAULT sy-uname
      !language           TYPE sylangu DEFAULT sy-langu
      !free_selections    TYPE rsds_texpr OPTIONAL
      !selection_table    TYPE rsparams_tt OPTIONAL
      !print_parameters   TYPE bapipripar OPTIONAL
      !archive_parameters TYPE bapiarcpar OPTIONAL
    RETURNING
      VALUE(job)          TYPE REF TO zif_job
*      VALUE(step_number)  TYPE bapixmjob-stepcount
    RAISING
      zcx_job .

  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter program | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter parameters | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter server | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter rfcdest | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter set_trace_on | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter stderr_in_joblog | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter stdout_in_joblog | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter wait_for_termination | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter user | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter job | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_job | <p class="shorttext synchronized" lang="en"></p>
  METHODS add_step_external_program
    IMPORTING
      !program              TYPE tbtcstep-program
      !parameters           TYPE tbtcstep-parameter OPTIONAL
      !server               TYPE tbtcstep-xpgtgtsys OPTIONAL
      !rfcdest              TYPE tbtcstep-xpgrfcdest OPTIONAL
      !set_trace_on         TYPE abap_bool DEFAULT abap_false
      !stderr_in_joblog     TYPE abap_bool DEFAULT abap_true
      !stdout_in_joblog     TYPE abap_bool DEFAULT abap_true
      !wait_for_termination TYPE abap_bool DEFAULT abap_false
      !user                 TYPE syuname DEFAULT sy-uname
    RETURNING
      VALUE(job)            TYPE REF TO zif_job
*      VALUE(step_number)    TYPE bapixmjob-stepcount
    RAISING
      zcx_job .

  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter command | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter operating_system | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter parameters | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter server | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter rfcdest | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter set_trace_on | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter stderr_in_joblog | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter stdout_in_joblog | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter wait_for_termination | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter user | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter job | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_job | <p class="shorttext synchronized" lang="en"></p>
  METHODS add_step_external_command
    IMPORTING
      !command              TYPE sxpgcolist-name
      !operating_system     TYPE syopsys DEFAULT sy-opsys
      !parameters           TYPE tbtcstep-parameter OPTIONAL
      !server               TYPE tbtcstep-xpgtgtsys OPTIONAL
      !rfcdest              TYPE tbtcstep-xpgrfcdest OPTIONAL
      !set_trace_on         TYPE abap_bool DEFAULT abap_false
      !stderr_in_joblog     TYPE abap_bool DEFAULT abap_true
      !stdout_in_joblog     TYPE abap_bool DEFAULT abap_true
      !wait_for_termination TYPE abap_bool DEFAULT abap_false
      !user                 TYPE syuname DEFAULT sy-uname
    RETURNING
      VALUE(job)            TYPE REF TO zif_job
*      VALUE(step_number)    TYPE bapixmjob-stepcount
    RAISING
      zcx_job .

  "! <p class="shorttext synchronized" lang="en">Start job at a given date/time</p>
  METHODS start_at
    IMPORTING
      !date                TYPE d
      !time                TYPE t
      !not_later_than_date TYPE tbtcjob-laststrtdt OPTIONAL   " laststrtdt
      !not_later_than_time TYPE tbtcjob-laststrttm OPTIONAL   " laststrttm
    RAISING
      zcx_job .

  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter first_date | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter first_time | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter skip_if_not_started_in_minutes | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter months | <p class="shorttext synchronized" lang="en">Every X months</p>
  "! @parameter weeks | <p class="shorttext synchronized" lang="en">Every X weeks</p>
  "! @parameter days | <p class="shorttext synchronized" lang="en">Every X days</p>
  "! @parameter hours | <p class="shorttext synchronized" lang="en">Every X hours</p>
  "! @parameter MINS | <p class="shorttext synchronized" lang="en">Every X minutes</p>
  "! @parameter calendar_id | <p class="shorttext synchronized" lang="en">Calendar ID for holiday</p>
  "! @parameter rule_if_date_falls_on_holiday | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_job | <p class="shorttext synchronized" lang="en"></p>
  METHODS start_periodically
    IMPORTING
      !first_date                     TYPE d
      !first_time                     TYPE t
      !skip_if_not_started_in_minutes TYPE i DEFAULT 0
      !months                         TYPE tbtcjob-prdmonths DEFAULT 0
      !weeks                          TYPE tbtcjob-prdweeks DEFAULT 0
      !days                           TYPE tbtcjob-prddays DEFAULT 0
      !hours                          TYPE tbtcjob-prdhours DEFAULT 0
      !mins                           TYPE tbtcjob-prdmins DEFAULT 0
      !calendar_id                    TYPE tbtcjob-calendarid OPTIONAL
      !rule_if_date_falls_on_holiday  TYPE ty_calendar_rule DEFAULT calendar_rule-process_always
    RAISING
      zcx_job .

  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter first_date | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter first_time | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter skip_if_not_started_in_minutes | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter months | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter calendar_id | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter nth_workday | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter start_on_workday_not_before | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_job | <p class="shorttext synchronized" lang="en"></p>
  METHODS start_monthly_nth_workday
    IMPORTING
      !first_date                     TYPE d
      !first_time                     TYPE t
      !skip_if_not_started_in_minutes TYPE i OPTIONAL
      !months                         TYPE tbtcjob-prdmonths DEFAULT 1
      !calendar_id                    TYPE tbtcjob-calendarid
      !nth_workday                    TYPE tbtcstrt-wdayno OPTIONAL
      !start_on_workday_not_before    TYPE tbtcstrt-notbefore DEFAULT sy-datum
    RAISING
      zcx_job .

  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter predecessor | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter predjob_checkstat | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_job | <p class="shorttext synchronized" lang="en"></p>
  METHODS start_after_job
    IMPORTING
      !predecessor       TYPE REF TO zif_job
      !predjob_checkstat TYPE tbtcstrt-checkstat DEFAULT abap_false
    RAISING
      zcx_job .

  "! <p class="shorttext synchronized" lang="en">Start job after event is triggered (SM62)</p>
  METHODS start_at_event
    IMPORTING
      !id       TYPE tbtcjob-eventid
      !param    TYPE tbtcjob-eventparm
      !periodic TYPE btch0000-char1 DEFAULT abap_false
    RAISING
      zcx_job .

  "! <p class="shorttext synchronized" lang="en">Start job when new operation mode starts (SM63)</p>
  METHODS start_at_opmode_switch
    IMPORTING
      !opmode          TYPE spfba-baname
      !opmode_periodic TYPE btch0000-char1
    RAISING
      zcx_job .

  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter check_actual_status | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter state | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_job | <p class="shorttext synchronized" lang="en"></p>
  METHODS get_state
    IMPORTING
      !check_actual_status TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(state)         TYPE btcstatus
    RAISING
      zcx_job .

ENDINTERFACE.
