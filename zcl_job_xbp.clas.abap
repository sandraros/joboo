class ZCL_JOB_XBP definition
  public
  final
  create public .

public section.

  interfaces ZIF_JOB .

  aliases AT_OPMODE
    for ZIF_JOB~AT_OPMODE .
  aliases AT_OPMODE_PERIODIC
    for ZIF_JOB~AT_OPMODE_PERIODIC .
  aliases CALENDAR_ID
    for ZIF_JOB~CALENDAR_ID .
  aliases CALENDAR_RULE
    for ZIF_JOB~CALENDAR_RULE .
  aliases CLASS
    for ZIF_JOB~CLASS .
  aliases COUNT
    for ZIF_JOB~COUNT .
  aliases DIRECT_START
    for ZIF_JOB~DIRECT_START .
  aliases DONT_RELEASE
    for ZIF_JOB~DONT_RELEASE .
  aliases EVENT_ID
    for ZIF_JOB~EVENT_ID .
  aliases EVENT_PARAM
    for ZIF_JOB~EVENT_PARAM .
  aliases EVENT_PERIODIC
    for ZIF_JOB~EVENT_PERIODIC .
  aliases JCLASS
    for ZIF_JOB~JCLASS .
  aliases LASTSTRTDT
    for ZIF_JOB~LASTSTRTDT .
  aliases LASTSTRTTM
    for ZIF_JOB~LASTSTRTTM .
  aliases NAME
    for ZIF_JOB~NAME .
  aliases PRDDAYS
    for ZIF_JOB~PRDDAYS .
  aliases PRDHOURS
    for ZIF_JOB~PRDHOURS .
  aliases PRDMINS
    for ZIF_JOB~PRDMINS .
  aliases PRDMONTHS
    for ZIF_JOB~PRDMONTHS .
  aliases PRDWEEKS
    for ZIF_JOB~PRDWEEKS .
  aliases PREDJOB_CHECKSTAT
    for ZIF_JOB~PREDJOB_CHECKSTAT .
  aliases PRED_JOBCOUNT
    for ZIF_JOB~PRED_JOBCOUNT .
  aliases PRED_JOBNAME
    for ZIF_JOB~PRED_JOBNAME .
  aliases RECIPIENT_OBJ
    for ZIF_JOB~RECIPIENT_OBJ .
  aliases SDLSTRTDT
    for ZIF_JOB~SDLSTRTDT .
  aliases SDLSTRTTM
    for ZIF_JOB~SDLSTRTTM .
  aliases STARTDATE_RESTRICTION
    for ZIF_JOB~STARTDATE_RESTRICTION .
  aliases START_ON_WORKDAY_NOT_BEFORE
    for ZIF_JOB~START_ON_WORKDAY_NOT_BEFORE .
  aliases START_ON_WORKDAY_NR
    for ZIF_JOB~START_ON_WORKDAY_NR .
  aliases STATE
    for ZIF_JOB~STATE .
  aliases STRTIMMED
    for ZIF_JOB~STRTIMMED .
  aliases TARGETGROUP
    for ZIF_JOB~TARGETGROUP .
  aliases TARGETSERVER
    for ZIF_JOB~TARGETSERVER .
  aliases TARGETSYSTEM
    for ZIF_JOB~TARGETSYSTEM .
  aliases WORKDAY_COUNT_DIRECTION
    for ZIF_JOB~WORKDAY_COUNT_DIRECTION .
  aliases ADD_STEP_ABAP
    for ZIF_JOB~ADD_STEP_ABAP .
  aliases GET_STATE
    for ZIF_JOB~GET_STATE .
  aliases SET_SERVER
    for ZIF_JOB~SET_SERVER .
  aliases SET_SERVER_GROUP
    for ZIF_JOB~SET_SERVER_GROUP .
  aliases SET_SERVER_OLD
    for ZIF_JOB~SET_SERVER_OLD .
  aliases START_AFTER_EVENT
    for ZIF_JOB~START_AFTER_EVENT .
  aliases START_AFTER_JOB
    for ZIF_JOB~START_AFTER_JOB .
  aliases START_AT
    for ZIF_JOB~START_AT .
  aliases START_AT_OPMODE_SWITCH
    for ZIF_JOB~START_AT_OPMODE_SWITCH .
  aliases START_IMMEDIATELY
    for ZIF_JOB~START_IMMEDIATELY .
  aliases START_MONTHLY_NTH_WORKDAY
    for ZIF_JOB~START_MONTHLY_NTH_WORKDAY .
  aliases START_PERIODICALLY
    for ZIF_JOB~START_PERIODICALLY .
  aliases TY_CALENDAR_RULE
    for ZIF_JOB~TY_CALENDAR_RULE .
  aliases TY_US_REPEATING_PERIOD
    for ZIF_JOB~TY_US_REPEATING_PERIOD .

  data XMI type ref to ZCL_XMI read-only .
  data EXTUSER type BAPIXMLOGR-EXTUSER read-only .

  methods Z .
  methods CONSTRUCTOR
    importing
      !XMI type ref to ZCL_XMI
      !NAME type BTCJOB
      !EXTUSER type BAPIXMLOGR-EXTUSER
      !CLASS type BAPIXMJOB-JOBCLASS optional
    raising
      ZCX_JOB .
  PRIVATE SECTION.

    METHODS close
      RAISING
        zcx_job_close .
ENDCLASS.



CLASS ZCL_JOB_XBP IMPLEMENTATION.


  METHOD close.
    CALL FUNCTION 'BAPI_XBP_JOB_CLOSE'
      EXPORTING
        jobname                  = me->name
        jobcount                 = me->count
*    external_user_name       = external_user_name
*   TARGET_SERVER            = TARGET_SERVER
*   RECIPIENT_OBJ            = RECIPIENT_OBJ
*   RECIPIENT                = RECIPIENT
* IMPORTING
*   RETURN                   = RETURN
              .
  ENDMETHOD.


  METHOD constructor.
    DATA: return TYPE bapiret2.

    super->constructor( ).

    me->xmi = xmi.
    me->name = name.
    me->extuser = extuser.
    me->jclass = class.

    CALL FUNCTION 'BAPI_XBP_JOB_OPEN'
      DESTINATION me->xmi->rfcdest
      EXPORTING
        jobname            = name
        external_user_name = extuser
        jobclass           = class
      IMPORTING
        jobcount           = me->count
        return             = return
      EXCEPTIONS
        COMMUNICATION_FAILURE = 1
        SYSTEM_FAILURE = 2
        others = 3.

    IF sy-subrc <> 0.
      " TODO
    ENDIF.
    IF return IS NOT INITIAL.
      zcx_job=>raise( return ).
    ENDIF.

  ENDMETHOD.


  METHOD z.

  ENDMETHOD.


  METHOD add_step_abap.
    DATA: return TYPE bapiret2,
    PRINT_PARAMETERS2 type BAPIXMPRNT,
          ARCHIVE_PARAMETERS2 type BAPIXMARCH,
          ALLPRIPAR type BAPIPRIPAR,
          ALLARCPAR type BAPIARCPAR,
          FREE_SELINFO TYPE  RSDSRANGE_T_SSEL.

CALL FUNCTION 'BAPI_XBP_JOB_ADD_ABAP_STEP'
  DESTINATION me->xmi->rfcdest
  EXPORTING
    jobname                  = name
    jobcount                 = count
    external_user_name       = extuser
    abap_program_name        = report
   ABAP_VARIANT_NAME        = variant
   SAP_USER_NAME            = user
   LANGUAGE                 = language
   PRINT_PARAMETERS         = PRINT_PARAMETERS2
   ARCHIVE_PARAMETERS       = ARCHIVE_PARAMETERS2
   ALLPRIPAR                = ALLPRIPAR
   ALLARCPAR                = ALLARCPAR
   FREE_SELINFO             = FREE_SELINFO
 IMPORTING
   STEP_NUMBER              = STEP_NUMBER
   RETURN                   = RETURN
 TABLES
   SELINFO                  = selection_table
      EXCEPTIONS
        COMMUNICATION_FAILURE = 1
        SYSTEM_FAILURE = 2
        others = 3.

  ENDMETHOD.


method ZIF_JOB~ADD_STEP_EXTERNAL_COMMAND.
  DATA: return TYPE bapiret2,
        program TYPE TBTCSTEP-PROGRAM.

  " TODO appeler SXPG_COMMAND_GET pour convertir COMMAND/OPERATING_SYSTEM en PROGRAM

  zif_job~add_step_external_program(
    EXPORTING
      program              = program
      parameters           = parameters
      server               = server
      rfcdest              = rfcdest
      set_trace_on         = ABAP_FALSE
      stderr_in_joblog     = ABAP_TRUE
      stdout_in_joblog     = ABAP_TRUE
      wait_for_termination = ABAP_FALSE
      user                 = SY-UNAME
    RECEIVING
      step_number          = step_number
  ).

endmethod.


METHOD zif_job~add_step_external_program.
  DATA: return TYPE bapiret2.

  CALL FUNCTION 'BAPI_XBP_JOB_ADD_EXT_STEP'
    DESTINATION me->xmi->rfcdest
    EXPORTING
      jobname                = me->name
      jobcount               = me->count
      external_user_name     = me->extuser
      ext_program_name       = program
      ext_program_parameters = parameters
      wait_for_termination   = abap_true
      target_host            = server
      sap_user_name          = user
    IMPORTING
      step_number            = step_number
      return                 = return
    EXCEPTIONS
      communication_failure  = 1
      system_failure         = 2
      OTHERS                 = 3.

ENDMETHOD.


  METHOD get_state.
    DATA: return TYPE bapiret2.

  if check_actual_status = abap_false.

CALL FUNCTION 'BAPI_XBP_JOB_STATUS_GET'
  DESTINATION me->xmi->rfcdest
  EXPORTING
    jobname                  = name
    jobcount                 = count
    external_user_name       = extuser
 IMPORTING
   STATUS                   = STATE
   RETURN                   = RETURN
*   HAS_CHILD                = HAS_CHILD
      EXCEPTIONS
        COMMUNICATION_FAILURE = 1
        SYSTEM_FAILURE = 2
        others = 3.

else.

CALL FUNCTION 'BAPI_XBP_JOB_STATUS_CHECK'
  DESTINATION me->xmi->rfcdest
  EXPORTING
    jobname                      = name
    jobcount                     = count
    external_user_name           = extuser
 IMPORTING
*   STATUS_ACCORDING_TO_DB       = STATUS_ACCORDING_TO_DB
   ACTUAL_STATUS                = state
   RETURN                       = RETURN
      EXCEPTIONS
        COMMUNICATION_FAILURE = 1
        SYSTEM_FAILURE = 2
        others = 3.

     endif.

  ENDMETHOD.


  METHOD set_server.
    me->targetserver = server.
    CLEAR : me->targetgroup, me->targetsystem.
  ENDMETHOD.


  METHOD set_server_group.
    me->targetgroup = server_group.
    CLEAR : me->targetserver, me->targetsystem.
  ENDMETHOD.


  METHOD set_server_old.
    me->targetsystem = server_old.
    CLEAR : me->targetserver, me->targetgroup.
  ENDMETHOD.


  METHOD start_after_event.
  ENDMETHOD.


  METHOD start_after_job.
  ENDMETHOD.


  METHOD start_at.
  ENDMETHOD.


  METHOD START_AT_OPMODE_SWITCH.
  ENDMETHOD.


  METHOD start_immediately.
    DATA: return TYPE bapiret2.

    if ERROR_IF_CANT_START_IMMED = abap_true.

CALL FUNCTION 'BAPI_XBP_JOB_START_IMMEDIATELY'
  DESTINATION me->xmi->rfcdest
  EXPORTING
    jobname                  = name
    jobcount                 = count
    external_user_name       = extuser
    target_server            = targetserver
   TARGET_GROUP             = TARGETGROUP
 IMPORTING
   RETURN                   = RETURN
      EXCEPTIONS
        COMMUNICATION_FAILURE = 1
        SYSTEM_FAILURE = 2
        others = 3.

else.

  CALL FUNCTION 'BAPI_XBP_JOB_START_ASAP'
  DESTINATION me->xmi->rfcdest
    EXPORTING
      jobname                  = name
      jobcount                 = count
      external_user_name       = extuser
      target_server            = targetserver
     TARGET_GROUP             = TARGETGROUP
   IMPORTING
     RETURN                   = RETURN
      EXCEPTIONS
        COMMUNICATION_FAILURE = 1
        SYSTEM_FAILURE = 2
        others = 3.

  endif.
  ENDMETHOD.


  method start_monthly_nth_workday.
  ENDMETHOD.


  METHOD start_periodically.
  ENDMETHOD.
ENDCLASS.
