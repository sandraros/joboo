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
      a TYPE btcjobclas VALUE tybtc_jobclass_a,
      b TYPE btcjobclas VALUE tybtc_jobclass_b,
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
  DATA name TYPE btcjob READ-ONLY .
  DATA count TYPE btcjobcnt READ-ONLY .
  DATA jclass TYPE btcjobclas READ-ONLY .
  DATA at_opmode TYPE spfba-baname READ-ONLY .
  DATA at_opmode_periodic TYPE btch0000-char1 READ-ONLY .
  DATA event_id TYPE tbtcjob-eventid READ-ONLY .
  DATA event_param TYPE tbtcjob-eventparm READ-ONLY .
  DATA event_periodic TYPE btch0000-char1 READ-ONLY .
  DATA sdlstrtdt TYPE tbtcjob-sdlstrtdt READ-ONLY .
  DATA sdlstrttm TYPE tbtcjob-sdlstrttm READ-ONLY .
  DATA laststrtdt TYPE tbtcjob-laststrtdt READ-ONLY .
  DATA laststrttm TYPE tbtcjob-laststrttm READ-ONLY .
  DATA prddays TYPE tbtcjob-prddays READ-ONLY .
  DATA prdhours TYPE tbtcjob-prdhours READ-ONLY .
  DATA prdmins TYPE tbtcjob-prdmins READ-ONLY .
  DATA prdmonths TYPE tbtcjob-prdmonths READ-ONLY .
  DATA prdweeks TYPE tbtcjob-prdweeks READ-ONLY .
  DATA calendar_id TYPE tbtcjob-calendarid READ-ONLY .
  DATA startdate_restriction TYPE tbtcjob-prdbehav READ-ONLY .
  DATA start_on_workday_not_before TYPE tbtcstrt-notbefore READ-ONLY .
  DATA start_on_workday_nr TYPE tbtcstrt-wdayno READ-ONLY .
  DATA workday_count_direction TYPE tbtcstrt-wdaycdir READ-ONLY .
  DATA predjob_checkstat TYPE tbtcstrt-checkstat READ-ONLY .
  DATA pred_jobcount TYPE tbtcjob-jobcount READ-ONLY .
  DATA pred_jobname TYPE tbtcjob-jobname READ-ONLY .
  DATA strtimmed TYPE btch0000-char1 READ-ONLY .
  DATA direct_start TYPE btch0000-char1 READ-ONLY .
  DATA targetsystem TYPE msxxlist-name READ-ONLY .
  DATA targetserver TYPE btctgtsrvr-srvname READ-ONLY .
  DATA targetgroup TYPE bpsrvgrp READ-ONLY .
  DATA recipient_obj TYPE swotobjid READ-ONLY .
  DATA dont_release TYPE btch0000-char1 READ-ONLY .

  METHODS set_server_old
    IMPORTING
      !server_old TYPE msxxlist-name
    RETURNING
      VALUE(job)  TYPE REF TO zif_job.
  METHODS set_server
    IMPORTING
      !server    TYPE btctgtsrvr-srvname
    RETURNING
      VALUE(job) TYPE REF TO zif_job .
  METHODS set_server_group
    IMPORTING
      !server_group TYPE bpsrvgrp
    RETURNING
      VALUE(job)    TYPE REF TO zif_job .
  TYPE-POOLS abap .
  METHODS start_immediately
    IMPORTING
      !error_if_cant_start_immed TYPE abap_bool DEFAULT abap_false
    RAISING
      zcx_job .
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
  METHODS start_at
    IMPORTING
      !date                TYPE d
      !time                TYPE t
      !not_later_than_date TYPE tbtcjob-laststrtdt OPTIONAL   " laststrtdt
      !not_later_than_time TYPE tbtcjob-laststrttm OPTIONAL   " laststrttm
    RETURNING
      VALUE(job)           TYPE REF TO zif_job
    RAISING
      zcx_job .
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
      !rule_if_date_falls_on_holiday  TYPE ty_calendar_rule OPTIONAL
    RETURNING
      VALUE(job)                      TYPE REF TO zif_job
    RAISING
      zcx_job .
  METHODS start_monthly_nth_workday
    IMPORTING
      !first_date                     TYPE d
      !first_time                     TYPE t
      !skip_if_not_started_in_minutes TYPE i OPTIONAL
      !months                         TYPE tbtcjob-prdmonths DEFAULT 1
      !calendar_id                    TYPE tbtcjob-calendarid
      !nth_workday                    TYPE tbtcstrt-wdayno OPTIONAL
      !start_on_workday_not_before    TYPE tbtcstrt-notbefore DEFAULT sy-datum
    RETURNING
      VALUE(job)                      TYPE REF TO zif_job
    RAISING
      zcx_job .
  METHODS set_successor_job
    IMPORTING
      !successor         TYPE REF TO zif_job
      !checkstat         TYPE tbtcstrt-checkstat DEFAULT abap_false
    RETURNING
      VALUE(job)         TYPE REF TO zif_job
    RAISING
      zcx_job.
  METHODS start_after_job
    IMPORTING
      !predecessor       TYPE REF TO zif_job
      !predjob_checkstat TYPE tbtcstrt-checkstat DEFAULT abap_false
    RETURNING
      VALUE(job)          TYPE REF TO zif_job
    RAISING
      zcx_job .
  METHODS start_after_event
    IMPORTING
      !id        TYPE tbtcjob-eventid
      !param     TYPE tbtcjob-eventparm
      !periodic  TYPE btch0000-char1 DEFAULT abap_false
    RETURNING
      VALUE(job) TYPE REF TO zif_job
    RAISING
      zcx_job .
  METHODS start_at_opmode_switch
    IMPORTING
      !opmode          TYPE spfba-baname
      !opmode_periodic TYPE btch0000-char1
    RETURNING
      VALUE(job)       TYPE REF TO zif_job
    RAISING
      zcx_job .

  METHODS get_state
    IMPORTING
      !check_actual_status TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(state)         TYPE btcstatus
    RAISING
      zcx_job .
ENDINTERFACE.
