"! Start methods:
"! <ul>
"! <li></li>
"! </ul>
"! <p class="shorttext synchronized" lang="en">Background Job Low-Level Operations</p>
INTERFACE zif_job_llop
  PUBLIC .

  METHODS job_open
    IMPORTING
      jobname        TYPE btcjob
      jobclass       TYPE btcjobclas
      check_jobclass TYPE abap_bool
    EXPORTING
      jobcount       TYPE btcjobcnt
      info           TYPE i
    CHANGING
      ret            TYPE i
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
  "! @parameter this_routine | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter step_number | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_job | <p class="shorttext synchronized" lang="en"></p>
  METHODS job_submit
    IMPORTING
      !jobname                     TYPE btcjob
      !jobcount                    TYPE btcjobcnt
      !arcparams                   TYPE arc_params OPTIONAL
      !authcknam                   TYPE tbtcjob-authcknam
      !commandname                 TYPE sxpgcolist-name OPTIONAL
      !operatingsystem             TYPE sy-opsys OPTIONAL
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
      !this_routine                TYPE symsgv
    EXPORTING
      !step_number                 TYPE tbtcjob-stepcount
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
      this_routine    TYPE symsgv
    RAISING
      zcx_job.

  METHODS job_close
    IMPORTING
      jobname                     TYPE btcjob
      jobcount                    TYPE btcjobcnt
      at_opmode                   TYPE spfba-baname
      at_opmode_periodic          TYPE btch0000-char1
      event_id                    TYPE tbtcjob-eventid
      event_param                 TYPE tbtcjob-eventparm
      event_periodic              TYPE btch0000-char1
      sdlstrtdt                   TYPE d
      sdlstrttm                   TYPE t
      laststrtdt                  TYPE d
      laststrttm                  TYPE t
      prddays                     TYPE tbtcjob-prddays
      prdhours                    TYPE tbtcjob-prdhours
      prdmins                     TYPE tbtcjob-prdmins
      prdmonths                   TYPE tbtcjob-prdmonths
      prdweeks                    TYPE tbtcjob-prdweeks
      calendar_id                 TYPE tbtcjob-calendarid
      startdate_restriction       TYPE tbtcjob-prdbehav
      start_on_workday_not_before TYPE d
      start_on_workday_nr         TYPE tbtcstrt-wdayno
      workday_count_direction     TYPE tbtcstrt-wdaycdir
      predjob_checkstat           TYPE tbtcstrt-checkstat
      pred_jobcount               TYPE tbtcjob-jobcount
      pred_jobname                TYPE tbtcjob-jobname
      strtimmed                   TYPE btch0000-char1
      direct_start                TYPE btch0000-char1
      recipient_obj               TYPE swotobjid
      targetsystem                TYPE msxxlist-name
      targetserver                TYPE btctgtsrvr-srvname
      targetgroup                 TYPE bpsrvgrp
      dont_release                TYPE btch0000-char1
*      EXPORTING
*        job_was_released            TYPE any
*      CHANGING
*        ret                         TYPE any
    RAISING
      zcx_job.

ENDINTERFACE.
