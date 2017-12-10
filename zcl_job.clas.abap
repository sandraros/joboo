class ZCL_JOB definition
  public
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
  aliases DIRECTION
    for ZIF_JOB~DIRECTION .
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
  aliases ADD_STEP_EXTERNAL_COMMAND
    for ZIF_JOB~ADD_STEP_EXTERNAL_COMMAND .
  aliases ADD_STEP_EXTERNAL_PROGRAM
    for ZIF_JOB~ADD_STEP_EXTERNAL_PROGRAM .
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
  aliases TY_US_WORKING_DAYS
    for ZIF_JOB~TY_US_WORKING_DAYS .
  aliases TY_UT_JOB
    for ZIF_JOB~TY_UT_JOB .
  aliases TY_WORKDAY_COUNT_DIRECTION
    for ZIF_JOB~TY_WORKDAY_COUNT_DIRECTION .

  constants NO_DATE type D value SPACE. "#EC NOTEXT
  constants NO_TIME type T value SPACE. "#EC NOTEXT

  methods CONSTRUCTOR
    importing
      !NAME type BTCJOB
      !USER type SYUNAME default SY-UNAME
      !CLASS type BAPIXMJOB-JOBCLASS default ZIF_JOB=>CLASS-C
      !CHECK_JOBCLASS type abap_bool DEFAULT abap_false
    raising
      ZCX_JOB .
private section.

  methods CONVERT_SY_TO_BAPIRET2
    returning
      value(BAPIRET2) type BAPIRET2 .
  methods CHECK_RET_CODE
    importing
      !RET type I
      !THIS_ROUTINE type CSEQUENCE
    raising
      ZCX_JOB .
  methods CLOSE
    raising
      ZCX_JOB .
  methods SUBMIT
    importing
      !ARCPARAMS type ARC_PARAMS optional
      !AUTHCKNAM type TBTCJOB-AUTHCKNAM
      !COMMANDNAME type SXPGCOLIST-NAME optional
      !OPERATINGSYSTEM type SY-OPSYS optional
      !EXTPGM_NAME type TBTCSTEP-PROGRAM optional
      !EXTPGM_PARAM type TBTCSTEP-PARAMETER optional
      !EXTPGM_SET_TRACE_ON type BTCH0000-CHAR1 optional
      !EXTPGM_STDERR_IN_JOBLOG type BTCH0000-CHAR1 default 'X'
      !EXTPGM_STDOUT_IN_JOBLOG type BTCH0000-CHAR1 default 'X'
      !EXTPGM_SYSTEM type TBTCSTEP-XPGTGTSYS optional
      !EXTPGM_RFCDEST type TBTCSTEP-XPGRFCDEST optional
      !EXTPGM_WAIT_FOR_TERMINATION type BTCH0000-CHAR1 optional
      !LANGUAGE type SY-LANGU default SY-LANGU
      !PRIPARAMS type PRI_PARAMS optional
      !REPORT type SY-REPID optional
      !VARIANT type RALDB-VARIANT optional
      !THIS_ROUTINE type SYMSGV
    exporting
      !STEP_NUMBER type TBTCJOB-STEPCOUNT
    raising
      ZCX_JOB .
  methods PROCESS_PRINT_ARCHIVE_PARAMS
    importing
      value(PRINT_PARAMETERS) type BAPIPRIPAR
      value(ARCHIVE_PARAMETERS) type BAPIARCPAR
      !REPORT type PROGRAM
      !USER type SYUNAME
    exporting
      !ARCPARAMS type ARC_PARAMS
      !PRIPARAMS type PRI_PARAMS
    raising
      ZCX_JOB .
ENDCLASS.



CLASS ZCL_JOB IMPLEMENTATION.


  METHOD add_step_abap.
    CONSTANTS : this_routine TYPE symsgv VALUE 'ADD_STEP_ABAP'.
    DATA: arcparams TYPE arc_params,
          priparams TYPE pri_params,
          dummy TYPE string.


    process_print_archive_params(
      EXPORTING
        archive_parameters = archive_parameters
        print_parameters = print_parameters
        report = report
        user = user
      IMPORTING
        arcparams = arcparams
        priparams = priparams ).

    IF selection_table IS INITIAL AND free_selections IS INITIAL.

      submit(
EXPORTING
  arcparams   = arcparams
  authcknam   = user
  language    = language
  priparams   = priparams
  report      = report
  variant     = variant
  this_routine = this_routine
IMPORTING
  step_number = step_number ).

    ELSEIF selection_table IS NOT INITIAL AND variant IS INITIAL AND user IS INITIAL.

      SUBMIT (report)
            VIA JOB me->name NUMBER me->count
            WITH SELECTION-TABLE selection_table
            TO SAP-SPOOL
            WITHOUT SPOOL DYNPRO
            ARCHIVE PARAMETERS arcparams
            SPOOL PARAMETERS priparams
            AND RETURN.
      IF sy-subrc <> 0.
        MESSAGE e027(bt) WITH report INTO dummy. " Failed to create job step & (see system log)
        zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).
      ENDIF.

    ELSEIF variant IS INITIAL.

      SUBMIT (report)
            VIA JOB me->name NUMBER me->count
            USER user
            WITH SELECTION-TABLE selection_table
            TO SAP-SPOOL
            WITHOUT SPOOL DYNPRO
            ARCHIVE PARAMETERS arcparams
            SPOOL PARAMETERS priparams
            AND RETURN.
      IF sy-subrc <> 0.
        MESSAGE e027(bt) WITH report INTO dummy. " Failed to create job step & (see system log)
        zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).
      ENDIF.

*'Error with free selections'  "#EC NOTEXT
*'Error with free selections / static variant'  "#EC NOTEXT
    ELSEIF user IS INITIAL.

      SUBMIT (report)
            VIA JOB me->name NUMBER me->count
            USING SELECTION-SET variant
            WITH SELECTION-TABLE selection_table
            TO SAP-SPOOL
            WITHOUT SPOOL DYNPRO
            ARCHIVE PARAMETERS arcparams
            SPOOL PARAMETERS priparams
            AND RETURN.
      IF sy-subrc <> 0.
        MESSAGE e027(bt) WITH report INTO dummy. " Failed to create job step & (see system log)
        zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).
      ENDIF.

    ELSE.

      SUBMIT (report)
            VIA JOB me->name NUMBER me->count
            USER user
            USING SELECTION-SET variant
            WITH SELECTION-TABLE selection_table
            TO SAP-SPOOL
            WITHOUT SPOOL DYNPRO
            ARCHIVE PARAMETERS arcparams
            SPOOL PARAMETERS priparams
            AND RETURN.
      IF sy-subrc <> 0.
        MESSAGE e027(bt) WITH report INTO dummy. " Failed to create job step & (see system log)
        zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


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


  METHOD close.
    CONSTANTS: this_routine TYPE symsgv VALUE 'JOB_CLOSE'.
    DATA: job_was_released TYPE btch0000-char1,
          ret              TYPE i,
          dummy            TYPE string.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount                    = me->count
        jobname                     = me->name
*       --------------- mode ----------------
        at_opmode                   = at_opmode
        at_opmode_periodic          = at_opmode_periodic
*       --------------- événement --------------
        event_id                    = event_id
        event_param                 = event_param
        event_periodic              = event_periodic
*       --------------- périodique --------------
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
*       --------------- job prédécesseur ---------
        predjob_checkstat           = predjob_checkstat
        pred_jobcount               = pred_jobcount
        pred_jobname                = pred_jobname
*       ------------------------------------------
        strtimmed                   = strtimmed
        direct_start                = direct_start
*       ------------------------------------------
        recipient_obj               = recipient_obj
*       ------------------------------------------
        targetsystem                = targetsystem
        targetserver                = targetserver
        targetgroup                 = targetgroup
*       ------------------------------------------
        dont_release                = dont_release
      IMPORTING
        job_was_released            = job_was_released
      CHANGING
        ret                         = ret
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
    IF sy-subrc = 0.
      IF dont_release = abap_false AND job_was_released = abap_false.
        IF 0 = 1. MESSAGE e054(xm). ENDIF. " No authorization to release a job
        MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_no_release_privileg.
      ENDIF.
    ELSE.
      CASE sy-subrc.
        WHEN 1.
          IF 0 = 1. MESSAGE e066(xm). ENDIF. " Immediate start not currently possible
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_no_immediate_start_poss INTO dummy.
        WHEN 2.
          IF 0 = 1. MESSAGE e068(xm). ENDIF. " Invalid date or invalid time specified
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_invalid_date_time INTO dummy.
        WHEN 3.
          IF 0 = 1. MESSAGE e046(xm). ENDIF. " Job name missing (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_jobname_missing WITH this_routine INTO dummy.
        WHEN 4.
          check_ret_code( ret = ret this_routine = this_routine ).
        WHEN 5.
          IF 0 = 1. MESSAGE e059(xm). ENDIF. " The specified job does not have any steps
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_no_jobsteps INTO dummy.
        WHEN 6.
          IF 0 = 1. MESSAGE e049(xm). ENDIF. " Job does not exist (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_job_does_not_exist WITH this_routine INTO dummy.
        WHEN 7.
          IF 0 = 1. MESSAGE e261(xm). ENDIF. " Could not lock job &2, job count &3
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_job_lock_failed WITH space name count.
        WHEN 8.
          IF 0 = 1. MESSAGE e069(xm). ENDIF. " Invalid server name specified (server name = &1)
          IF targetsystem IS NOT INITIAL.
            MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_invalid_server_name WITH targetsystem.
          ELSEIF targetserver IS NOT INITIAL.
            MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_invalid_server_name WITH targetserver.
          ELSE.
            MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_invalid_server_name WITH targetgroup.
          ENDIF.
        WHEN 9.
          IF 0 = 1. MESSAGE e034(xm). ENDIF. " Internal problem (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_problem_detected WITH this_routine.
      ENDCASE.

      zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    CONSTANTS: this_routine TYPE symsgv VALUE 'JOB_OPEN'.
    DATA: info  TYPE i,
          ret   TYPE i,
          dummy TYPE string.

    super->constructor( ).

    me->name = to_upper( name ).
    me->jclass = class.
    me->sdlstrtdt = no_date. " date with spaces instead of zeroes, as defined in JOB_CLOSE
    me->sdlstrttm = no_time. " time with spaces instead of zeroes, as defined in JOB_CLOSE

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = name
        jobclass         = class    " Job classification
        check_jobclass   = check_jobclass
      IMPORTING
        jobcount         = me->count
        info             = info    " ID Number of Background Job
      CHANGING
        ret              = ret    " Special Additional Error Code
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    " the following messages are based on BAPI_XBP_JOB_OPEN error handling.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          check_ret_code( ret = ret this_routine = this_routine ).
        WHEN 2.
          IF 0 = 1. MESSAGE e202(xm). ENDIF. " Invalid new job data
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_invalid_new_jobdata INTO dummy.
        WHEN 3.
          IF 0 = 1. MESSAGE e046(xm). ENDIF. " Job name missing (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_jobname_missing WITH this_routine INTO dummy.
        WHEN 4.
          IF 0 = 1. MESSAGE e034(xm). ENDIF. " Internal problem (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_problem_detected WITH this_routine INTO dummy.
      ENDCASE.

      zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).

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


  METHOD get_state.
    DATA: job_read_jobhead TYPE tbtcjob,
    dummy TYPE string.

    IF check_actual_status = abap_false.

      CALL FUNCTION 'BP_JOB_READ'
        EXPORTING
          job_read_jobcount           = count
          job_read_jobname            = name
          job_read_opcode             = tybtc_read_jobhead_only
*       JOB_STEP_NUMBER             = JOB_STEP_NUMBER
       IMPORTING
         job_read_jobhead            = job_read_jobhead
*       JOBLOG_ATTRIBUTES           = JOBLOG_ATTRIBUTES
*       EPP_ATTRIBUTES              = EPP_ATTRIBUTES
*     TABLES
*       JOB_READ_STEPLIST           = JOB_READ_STEPLIST
*       SPOOL_ATTRIBUTES            = SPOOL_ATTRIBUTES
*     CHANGING
*       RET                         = RET
       EXCEPTIONS
*       INVALID_OPCODE              = 1
         job_doesnt_exist            = 2
         job_doesnt_have_steps       = 3
         OTHERS = 99.

      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 2.
            IF 0 = 1. MESSAGE e049(xm). ENDIF. " Job does not exist (function &1)
            MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_job_does_not_exist WITH 'BP_JOB_READ' INTO dummy.
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
*         invalid_dialog_type          = 3
          job_does_not_exist           = 4
          no_check_privilege_given     = 5
          ready_switch_too_dangerous   = 0 "normal situation below 60 seconds
          OTHERS                       = 7.

      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 4.
            IF 0 = 1. MESSAGE e049(xm). ENDIF. " Job does not exist (function &1)
            MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_job_does_not_exist WITH 'BP_JOB_CHECKSTATE' INTO dummy.
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


  METHOD process_print_archive_params.
    CONSTANTS : this_routine        TYPE symsgv VALUE 'ADD_STEP_ABAP',
                c_char_unknown      TYPE c VALUE '_', "Unbekannt C
                c_int_unknown       TYPE i VALUE -1,  "Unbekannt I
                c_num1_unknown      TYPE n VALUE '0',"Unbekannt N(1)
                c_char_space_request TYPE c VALUE '$'. "will be SPACE
    DATA dummy TYPE string.

    " code taken from subroutine INIT_PRINT_PARAMETERS in program SAPLSXBP.

* for some values, we have to rely on the caller for
* the correct initialization because SPACE is not the
* default value

    IF print_parameters-primm IS INITIAL OR
       print_parameters-primm = space.
      print_parameters-primm = c_char_unknown.
* caller must pass '$' if 'do not print immediately' shall be set
    ELSEIF print_parameters-primm = c_char_space_request.
      CLEAR print_parameters-primm.
    ENDIF.

    IF print_parameters-prrel IS INITIAL.
      print_parameters-prrel = c_char_unknown.
* caller must pass '$' if 'do not release after print' shall be set
    ELSEIF print_parameters-prrel = c_char_space_request.
      CLEAR print_parameters-prrel.
    ENDIF.

    IF print_parameters-prnew IS INITIAL.
      print_parameters-prnew = c_char_unknown.
* caller must pass '$' if 'append spool' shall be set
    ELSEIF print_parameters-prnew = c_char_space_request.
      CLEAR print_parameters-prnew.
    ENDIF.

    IF print_parameters-prsap IS INITIAL.
      print_parameters-prsap = c_char_unknown.
* caller must pass '$' if 'no SAP cover page' shall be set
    ELSEIF print_parameters-prsap = c_char_space_request.
      CLEAR print_parameters-prsap.
    ENDIF.

    IF print_parameters-prunx IS INITIAL.
      print_parameters-prunx = c_char_unknown.
* caller must pass '$' if 'no host spool cover page' shall be set
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
* caller must pass '$' if SPACE shall be set
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
* caller must pass '$' if SPACE shall be set
    ELSEIF print_parameters-prabt = c_char_space_request.
      CLEAR print_parameters-prabt.
    ENDIF.

    IF print_parameters-prber IS INITIAL.
      print_parameters-prber = c_char_unknown.
    ENDIF.

    IF print_parameters-prdsn IS INITIAL.
      print_parameters-prdsn = c_char_unknown.
    ENDIF.

*  IF print_parameters-ptype IS INITIAL OR
*     print_parameters-ptype = space.
    print_parameters-ptype = c_char_unknown.
*  ENDIF.

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
*       IN_ARCHIVE_PARAMETERS  = allpripar-
*       IN_PARAMETERS          = allpripar-
        layout                 = priparams-paart
        line_count             = priparams-linct
        line_size              = priparams-linsz
        list_name              = priparams-plist
        list_text              = priparams-prtxt
        mode                   = 'BATCH'
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
*       valid                  = valid_pri_params
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

    me->event_id = event_id.
    me->event_param = event_param.
    me->event_periodic = event_periodic.
    close( ).

  ENDMETHOD.


  METHOD start_after_job.

    pred_jobcount = job->count.
    pred_jobname = job->name.
    close( ).

  ENDMETHOD.


  METHOD start_at.

    me->sdlstrtdt = date.
    me->sdlstrttm = time.
    close( ).

  ENDMETHOD.


  METHOD start_at_opmode_switch.
    me->at_opmode = opmode.
    me->at_opmode_periodic = opmode_periodic.
    close( ).
  ENDMETHOD.


  METHOD start_immediately.

    me->strtimmed = abap_true.
    IF error_if_cant_start_immed = abap_true.
      me->direct_start = abap_true.
    ELSE.
      me->direct_start = abap_false.
    ENDIF.
    close( ).

  ENDMETHOD.


  METHOD start_monthly_nth_workday.
    DATA: tstmp   TYPE timestamp,
          tstmp_numc TYPE n LENGTH 14.

    me->sdlstrtdt = first_date.
    me->sdlstrttm = first_time.
    tstmp = first_date && first_time.
    tstmp_numc = tstmp = cl_abap_tstmp=>add( tstmp = tstmp secs = skip_if_not_started_in_minutes * 60 ).
*      CATCH cx_parameter_invalid_range.    " Parameter with Invalid Range
*      CATCH cx_parameter_invalid_type.    " Parameter with Invalid Type
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
    close( ).

  ENDMETHOD.


  METHOD start_periodically.
    DATA: tstmp   TYPE timestamp,
          tstmp_numc TYPE n LENGTH 14.

    me->sdlstrtdt = first_date.
    me->sdlstrttm = first_time.
    tstmp = first_date && first_time.
    tstmp_numc = tstmp = cl_abap_tstmp=>add( tstmp = tstmp secs = skip_if_not_started_in_minutes * 60 ).
*      CATCH cx_parameter_invalid_range.    " Parameter with Invalid Range
*      CATCH cx_parameter_invalid_type.    " Parameter with Invalid Type
    me->laststrtdt = tstmp_numc(8).
    me->laststrttm = tstmp_numc+8(6).
    me->prddays   = days  .
    me->prdhours  = hours .
    me->prdmins   = mins  .
    me->prdmonths = months.
    me->prdweeks  = weeks .
    me->startdate_restriction = rule_if_date_falls_on_holiday.
    me->calendar_id = calendar_id.
    close( ).

  ENDMETHOD.


  METHOD submit.
    DATA dummy TYPE string.

    CALL FUNCTION 'JOB_SUBMIT'
      EXPORTING
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
        jobcount                    = me->count
        jobname                     = me->name
        language                    = language
        priparams                   = priparams
        report                      = report
        variant                     = variant
      IMPORTING
        step_number                 = step_number
      EXCEPTIONS
        bad_priparams               = 1
        bad_xpgflags                = 2
        invalid_jobdata             = 3
        jobname_missing             = 4
        job_notex                   = 5
        job_submit_failed           = 6
        lock_failed                 = 7
        program_missing             = 8
        prog_abap_and_extpg_set     = 9.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          " should not happen, it should have been intercepted during
          " method process_print_archive_params.
          IF 0 = 1. MESSAGE e034(xm). ENDIF. " Internal problem (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_problem_detected WITH this_routine INTO dummy.
        WHEN 2.
          " TODO créer un message pour XPGFLAGS
        WHEN 3.
          IF 0 = 1. MESSAGE e202(xm). ENDIF. " Invalid new job data
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_invalid_new_jobdata INTO dummy.
        WHEN 4.
          IF 0 = 1. MESSAGE e046(xm). ENDIF. " Job name missing (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_jobname_missing WITH this_routine INTO dummy.
        WHEN 5.
          IF 0 = 1. MESSAGE e049(xm). ENDIF. " Job does not exist (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_job_does_not_exist WITH this_routine INTO dummy.
        WHEN 6.
          MESSAGE e027(bt) WITH report INTO dummy. " Failed to create job step & (see system log)
        WHEN 7.
          IF 0 = 1. MESSAGE e194(xm). ENDIF. " Job could not be locked
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_cant_enq_job INTO dummy.
        WHEN 8.
          IF 0 = 1. MESSAGE e050(xm). ENDIF. " Report or program not specified or name incomplete (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_progname_missing WITH this_routine INTO dummy.
        WHEN 9.
          " can't happen
          IF 0 = 1. MESSAGE e034(xm). ENDIF. " Internal problem (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_problem_detected WITH this_routine.
        WHEN 10.
          IF 0 = 1. MESSAGE e034(xm). ENDIF. " Internal problem (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_problem_detected WITH this_routine.
      ENDCASE.

      zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_job~ADD_STEP_EXTERNAL_command.
    CONSTANTS : this_routine TYPE symsgv VALUE 'ADD_STEP_EXTERNAL_COMMAND' ##NO_TEXT.

    CALL METHOD submit
      EXPORTING
        commandname                 = command
        extpgm_param                = parameters
        operatingsystem             = operating_system
        extpgm_rfcdest              = RFCDEST
        extpgm_set_trace_on         = set_trace_on
        extpgm_stderr_in_joblog     = stderr_in_joblog
        extpgm_stdout_in_joblog     = stdout_in_joblog
        extpgm_wait_for_termination = wait_for_termination
        authcknam                   = USER
        this_routine                = this_routine
      IMPORTING
        step_number                 = step_number.
  ENDMETHOD.


  METHOD zif_job~ADD_STEP_EXTERNAL_PROGRAM.
    CONSTANTS : this_routine TYPE symsgv VALUE 'ADD_STEP_EXTERNAL_PROGRAM' ##NO_TEXT.

    CALL METHOD submit
      EXPORTING
        extpgm_name                 = PROGRAM
        extpgm_param                = parameters
        extpgm_system               = server
        extpgm_rfcdest              = RFCDEST
        extpgm_set_trace_on         = set_trace_on
        extpgm_stderr_in_joblog     = stderr_in_joblog
        extpgm_stdout_in_joblog     = stdout_in_joblog
        extpgm_wait_for_termination = wait_for_termination
        authcknam                   = USER
        this_routine                = this_routine
      IMPORTING
        step_number                 = step_number.
  ENDMETHOD.
ENDCLASS.
