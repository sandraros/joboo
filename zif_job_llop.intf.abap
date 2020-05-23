"! Start methods:
"! <ul>
"! <li></li>
"! </ul>
"! <p class="shorttext synchronized" lang="en">Background Job Low-Level Operations</p>
INTERFACE zif_job_llop
  PUBLIC .

  CONSTANTS:
    "! Date with spaces instead of zeroes, as defined in JOB_CLOSE,
    "! which means "no date" transmitted via argument.
    no_date            TYPE d VALUE '        ' ##NO_TEXT,
    "! Time with spaces instead of zeroes, as defined in JOB_CLOSE,
    "! which means "no time" transmitted via argument.
    no_time            TYPE t VALUE '      ' ##NO_TEXT,
    btc_process_always TYPE btch0000-char1  VALUE ' '.

  METHODS job_open
    IMPORTING
      jobname        TYPE btcjob
      jobclass       TYPE btcjobclas OPTIONAL
      check_jobclass TYPE abap_bool OPTIONAL
    EXPORTING
      jobcount       TYPE btcjobcnt
      info           TYPE i
    CHANGING
      ret            TYPE i OPTIONAL
    RETURNING
      VALUE(subrc)   TYPE sysubrc
    RAISING
      zcx_job.

  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter ARCPARAMS | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter AUTHCKNAM | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter COMMANDNAME | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter OPERATINGSYSTEM | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter extpgm_name | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter extpgm_param | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter extpgm_set_trace_on | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter extpgm_stderr_in_joblog | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter extpgm_stdout_in_joblog | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter extpgm_system | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter extpgm_rfcdest | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter extpgm_wait_for_termination | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter language | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter PRIPARAMS | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter report | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter variant | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter THIS_PROCEDURE | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter step_number | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_job | <p class="shorttext synchronized" lang="en"></p>
  METHODS job_submit
    IMPORTING
      !jobname                     TYPE btcjob
      !jobcount                    TYPE btcjobcnt
      !arcparams                   TYPE arc_params OPTIONAL
      " Standard JOB_SUBMIT doesn't have a default AUTHCKNAM, I added it because I think it's a better choice
      !authcknam                   TYPE tbtcjob-authcknam DEFAULT sy-uname
      !commandname                 TYPE sxpgcolist-name OPTIONAL
      !operatingsystem             TYPE sy-opsys DEFAULT sy-opsys
      !extpgm_name                 TYPE tbtcstep-program OPTIONAL
      !extpgm_param                TYPE tbtcstep-parameter OPTIONAL
      !extpgm_set_trace_on         TYPE btch0000-char1 OPTIONAL
      !extpgm_stderr_in_joblog     TYPE btch0000-char1 DEFAULT 'X'
      !extpgm_stdout_in_joblog     TYPE btch0000-char1 DEFAULT 'X'
      !extpgm_system               TYPE tbtcstep-xpgtgtsys OPTIONAL
      !extpgm_rfcdest              TYPE tbtcstep-xpgrfcdest OPTIONAL
      !extpgm_wait_for_termination TYPE btch0000-char1 OPTIONAL
      !language                    TYPE sy-langu DEFAULT sy-langu
      !priparams                   TYPE pri_params OPTIONAL
      !report                      TYPE sy-repid OPTIONAL
      !variant                     TYPE raldb-variant OPTIONAL
      !this_procedure              TYPE symsgv OPTIONAL
    EXPORTING
      !step_number                 TYPE tbtcjob-stepcount
    RETURNING
      VALUE(subrc)                 TYPE sysubrc
    RAISING
      zcx_job .

  METHODS abap_submit
    IMPORTING
      jobname         TYPE btcjob
      jobcount        TYPE btcjobcnt
      selection_table TYPE rsparams_tt
      arcparams       TYPE arc_params
      user            TYPE syuname
      priparams       TYPE pri_params
      report          TYPE program
      variant         TYPE variant
      this_procedure  TYPE symsgv
    RETURNING
      VALUE(subrc)    TYPE sysubrc
    RAISING
      zcx_job.

  METHODS job_close
    IMPORTING
      jobname                     TYPE btcjob
      jobcount                    TYPE btcjobcnt
      at_opmode                   TYPE spfba-baname DEFAULT space
      at_opmode_periodic          TYPE btch0000-char1 DEFAULT space
      calendar_id                 TYPE tbtcjob-calendarid DEFAULT space
      event_id                    TYPE tbtcjob-eventid DEFAULT space
      event_param                 TYPE tbtcjob-eventparm DEFAULT space
      event_periodic              TYPE btch0000-char1 DEFAULT space
      laststrtdt                  TYPE tbtcjob-laststrtdt DEFAULT no_date
      laststrttm                  TYPE tbtcjob-laststrttm DEFAULT no_time
      prddays                     TYPE tbtcjob-prddays DEFAULT 0
      prdhours                    TYPE tbtcjob-prdhours DEFAULT 0
      prdmins                     TYPE tbtcjob-prdmins DEFAULT 0
      prdmonths                   TYPE tbtcjob-prdmonths DEFAULT 0
      prdweeks                    TYPE tbtcjob-prdweeks DEFAULT 0
      predjob_checkstat           TYPE tbtcstrt-checkstat DEFAULT space
      pred_jobcount               TYPE tbtcjob-jobcount DEFAULT space
      pred_jobname                TYPE tbtcjob-jobname DEFAULT space
      sdlstrtdt                   TYPE tbtcjob-sdlstrtdt DEFAULT no_date
      sdlstrttm                   TYPE tbtcjob-sdlstrttm DEFAULT no_time
      startdate_restriction       TYPE tbtcjob-prdbehav DEFAULT btc_process_always
      strtimmed                   TYPE btch0000-char1 DEFAULT space
      targetsystem                TYPE any DEFAULT space ##ADT_PARAMETER_UNTYPED
      start_on_workday_not_before TYPE tbtcstrt-notbefore DEFAULT sy-datum
      start_on_workday_nr         TYPE tbtcstrt-wdayno DEFAULT 0
      workday_count_direction     TYPE tbtcstrt-wdaycdir DEFAULT 0
      recipient_obj               TYPE swotobjid OPTIONAL
      targetserver                TYPE btctgtsrvr-srvname DEFAULT space
      dont_release                TYPE btch0000-char1 DEFAULT space
      targetgroup                 TYPE bpsrvgrp DEFAULT space
      direct_start                TYPE btch0000-char1 OPTIONAL
      inherit_recipient           TYPE btch0000-char1 OPTIONAL
      inherit_target              TYPE btch0000-char1 OPTIONAL
      register_child              TYPE btcchar1 DEFAULT abap_false
      time_zone                   TYPE tznzone OPTIONAL
      email_notification          TYPE btc_s_email OPTIONAL
    EXPORTING
      job_was_released            TYPE btch0000-char1
    CHANGING
      ret                         TYPE i OPTIONAL
    RETURNING
      VALUE(subrc)                TYPE sysubrc
    RAISING
      zcx_job.

ENDINTERFACE.
