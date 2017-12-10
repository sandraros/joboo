interface ZIF_JOB
  public .


  types:
    ty_ut_job TYPE TABLE OF REF TO zif_job .
  types:
    BEGIN OF ty_us_repeating_period,
      weeks  TYPE tbtcjob-prdweeks,
      days   TYPE tbtcjob-prddays,
      hours  TYPE tbtcjob-prdhours,
      mins   TYPE tbtcjob-prdmins,
    END OF ty_us_repeating_period .
  types:
    ty_calendar_rule TYPE c LENGTH 1 .
  types TY_WORKDAY_COUNT_DIRECTION type TBTCSTRT-WDAYCDIR .
  types:
    BEGIN OF ty_us_working_days,
      rule                    TYPE ty_calendar_rule,
      calendar_id             TYPE tbtcjob-calendarid,
      start_on_workday_nr     TYPE tbtcstrt-wdayno,
      workday_count_direction TYPE tbtcstrt-wdaycdir,
    END OF ty_us_working_days .

  constants:
    BEGIN OF class,
      a                    TYPE btcjobclas VALUE tybtc_jobclass_a,
      b                    TYPE btcjobclas VALUE tybtc_jobclass_b,
      c                    TYPE btcjobclas VALUE tybtc_jobclass_c,
    END OF class .
  constants:
    BEGIN OF state,
      running              TYPE btcstatus VALUE tybtc_running      ,
      ready                TYPE btcstatus VALUE tybtc_ready        ,
      scheduled            TYPE btcstatus VALUE tybtc_scheduled    ,
      intercepted          TYPE btcstatus VALUE tybtc_intercepted  ,
      released             TYPE btcstatus VALUE tybtc_released     ,
      aborted              TYPE btcstatus VALUE tybtc_aborted      ,
      finished             TYPE btcstatus VALUE tybtc_finished     ,
      put_active           TYPE btcstatus VALUE tybtc_put_active   ,
      unknown_state        TYPE btcstatus VALUE tybtc_unknown_state,
    END OF state .
  constants:
    BEGIN OF direction,
      btc_beginning_of_month TYPE ty_workday_count_direction VALUE tybtc_beginning_of_month,
      btc_end_of_month       TYPE ty_workday_count_direction VALUE tybtc_end_of_month,
    END OF direction .
  constants:
    BEGIN OF calendar_rule,
      dont_process_on_holiday TYPE ty_calendar_rule VALUE tybtc_dont_process_on_holiday,
      process_before_holiday  TYPE ty_calendar_rule VALUE tybtc_process_before_holiday,
      process_after_holiday   TYPE ty_calendar_rule VALUE tybtc_process_after_holiday,
      process_always          TYPE ty_calendar_rule VALUE tybtc_process_always,
    END OF calendar_rule .
  data NAME type BTCJOB read-only .
  data COUNT type BTCJOBCNT read-only .
  data JCLASS type BTCJOBCLAS read-only .
  data AT_OPMODE type SPFBA-BANAME read-only .
  data AT_OPMODE_PERIODIC type BTCH0000-CHAR1 read-only .
  data EVENT_ID type TBTCJOB-EVENTID read-only .
  data EVENT_PARAM type TBTCJOB-EVENTPARM read-only .
  data EVENT_PERIODIC type BTCH0000-CHAR1 read-only .
  data SDLSTRTDT type TBTCJOB-SDLSTRTDT read-only .
  data SDLSTRTTM type TBTCJOB-SDLSTRTTM read-only .
  data LASTSTRTDT type TBTCJOB-LASTSTRTDT read-only .
  data LASTSTRTTM type TBTCJOB-LASTSTRTTM read-only .
  data PRDDAYS type TBTCJOB-PRDDAYS read-only .
  data PRDHOURS type TBTCJOB-PRDHOURS read-only .
  data PRDMINS type TBTCJOB-PRDMINS read-only .
  data PRDMONTHS type TBTCJOB-PRDMONTHS read-only .
  data PRDWEEKS type TBTCJOB-PRDWEEKS read-only .
  data CALENDAR_ID type TBTCJOB-CALENDARID read-only .
  data STARTDATE_RESTRICTION type TBTCJOB-PRDBEHAV read-only .
  data START_ON_WORKDAY_NOT_BEFORE type TBTCSTRT-NOTBEFORE read-only .
  data START_ON_WORKDAY_NR type TBTCSTRT-WDAYNO read-only .
  data WORKDAY_COUNT_DIRECTION type TBTCSTRT-WDAYCDIR read-only .
  data PREDJOB_CHECKSTAT type TBTCSTRT-CHECKSTAT read-only .
  data PRED_JOBCOUNT type TBTCJOB-JOBCOUNT read-only .
  data PRED_JOBNAME type TBTCJOB-JOBNAME read-only .
  data STRTIMMED type BTCH0000-CHAR1 read-only .
  data DIRECT_START type BTCH0000-CHAR1 read-only .
  data TARGETSYSTEM type MSXXLIST-NAME read-only .
  data TARGETSERVER type BTCTGTSRVR-SRVNAME read-only .
  data TARGETGROUP type BPSRVGRP read-only .
  data RECIPIENT_OBJ type SWOTOBJID read-only .
  data DONT_RELEASE type BTCH0000-CHAR1 read-only .

  methods SET_SERVER_OLD
    importing
      !SERVER_OLD type MSXXLIST-NAME .
  methods SET_SERVER
    importing
      !SERVER type BTCTGTSRVR-SRVNAME .
  methods SET_SERVER_GROUP
    importing
      !SERVER_GROUP type BPSRVGRP .
  type-pools ABAP .
  methods START_IMMEDIATELY
    importing
      !ERROR_IF_CANT_START_IMMED type ABAP_BOOL default ABAP_FALSE
    raising
      ZCX_JOB .
  methods ADD_STEP_ABAP
    importing
      !REPORT type PROGRAM
      !VARIANT type VARIANT optional
      !USER type SYUNAME default SY-UNAME
      !LANGUAGE type SYLANGU default SY-LANGU
      !FREE_SELECTIONS type RSDS_TEXPR optional
      !SELECTION_TABLE type RSPARAMS_TT optional
      !PRINT_PARAMETERS type BAPIPRIPAR optional
      !ARCHIVE_PARAMETERS type BAPIARCPAR optional
    returning
      value(STEP_NUMBER) type BAPIXMJOB-STEPCOUNT
    raising
      ZCX_JOB .
  methods ADD_STEP_EXTERNAL_PROGRAM
    importing
      !PROGRAM type TBTCSTEP-PROGRAM
      !PARAMETERS type TBTCSTEP-PARAMETER optional
      !SERVER type TBTCSTEP-XPGTGTSYS OPTIONAL
      !RFCDEST type TBTCSTEP-XPGRFCDEST OPTIONAL
      !set_trace_on         TYPE abap_bool DEFAULT abap_false
      !stderr_in_joblog     TYPE abap_bool DEFAULT abap_true
      !stdout_in_joblog     TYPE abap_bool DEFAULT abap_true
      !wait_for_termination TYPE abap_bool DEFAULT abap_false
      !USER type SYUNAME default SY-UNAME
    returning
      value(STEP_NUMBER) type BAPIXMJOB-STEPCOUNT
    raising
      ZCX_JOB .
  methods ADD_STEP_EXTERNAL_COMMAND
    importing
      !COMMAND type SXPGCOLIST-NAME
      !OPERATING_SYSTEM type SYOPSYS default SY-OPSYS
      !PARAMETERS type TBTCSTEP-PARAMETER optional
      !SERVER type TBTCSTEP-XPGTGTSYS OPTIONAL
      !RFCDEST type TBTCSTEP-XPGRFCDEST OPTIONAL
      !set_trace_on         TYPE abap_bool DEFAULT abap_false
      !stderr_in_joblog     TYPE abap_bool DEFAULT abap_true
      !stdout_in_joblog     TYPE abap_bool DEFAULT abap_true
      !wait_for_termination TYPE abap_bool DEFAULT abap_false
      !USER type SYUNAME default SY-UNAME
    returning
      value(STEP_NUMBER) type BAPIXMJOB-STEPCOUNT
    raising
      ZCX_JOB .
  methods START_AT
    importing
      !DATE type D
      !TIME type T
      !NOT_LATER_THAN_DATE type TBTCJOB-LASTSTRTDT optional   " laststrtdt
      !NOT_LATER_THAN_TIME type TBTCJOB-LASTSTRTTM optional   " laststrttm
    raising
      ZCX_JOB .
  methods START_PERIODICALLY
    importing
      !FIRST_DATE type D
      !FIRST_TIME type T
      !SKIP_IF_NOT_STARTED_IN_MINUTES type I default 0
      !MONTHS type TBTCJOB-PRDMONTHS default 0
      !WEEKS type TBTCJOB-PRDWEEKS default 0
      !DAYS type TBTCJOB-PRDDAYS default 0
      !HOURS type TBTCJOB-PRDHOURS default 0
      !MINS type TBTCJOB-PRDMINS default 0
      !CALENDAR_ID type TBTCJOB-CALENDARID optional
      !RULE_IF_DATE_FALLS_ON_HOLIDAY type TY_CALENDAR_RULE optional
    raising
      ZCX_JOB .
  methods START_MONTHLY_NTH_WORKDAY
    importing
      !FIRST_DATE type D
      !FIRST_TIME type T
      !SKIP_IF_NOT_STARTED_IN_MINUTES type I optional
      !MONTHS type TBTCJOB-PRDMONTHS default 1
      !CALENDAR_ID type TBTCJOB-CALENDARID
      !NTH_WORKDAY type TBTCSTRT-WDAYNO optional
      !START_ON_WORKDAY_NOT_BEFORE type TBTCSTRT-NOTBEFORE default SY-DATUM
    raising
      ZCX_JOB .
  methods START_AFTER_JOB
    importing
      !JOB type ref to ZCL_JOB
      !PREDJOB_CHECKSTAT type TBTCSTRT-CHECKSTAT default ABAP_FALSE
    raising
      ZCX_JOB .
  methods START_AFTER_EVENT
    importing
      !ID type TBTCJOB-EVENTID
      !PARAM type TBTCJOB-EVENTPARM
      !PERIODIC type BTCH0000-CHAR1 default ABAP_FALSE
    raising
      ZCX_JOB .
  methods START_AT_OPMODE_SWITCH
    importing
      !OPMODE type SPFBA-BANAME
      !OPMODE_PERIODIC type BTCH0000-CHAR1
    raising
      ZCX_JOB .
  methods GET_STATE
    importing
      !CHECK_ACTUAL_STATUS type ABAP_BOOL default ABAP_FALSE
    returning
      value(STATE) type BTCSTATUS
    raising
      ZCX_JOB .
endinterface.
