*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
INCLUDE rsxmibapi_msg.

CLASS lcl_low_level_ops DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_job_llop.

    CLASS-METHODS convert_sy_to_bapiret2
      RETURNING
        VALUE(bapiret2) TYPE bapiret2 .

ENDCLASS.

CLASS lcl_low_level_ops IMPLEMENTATION.

  METHOD zif_job_llop~abap_submit.

    DATA: dummy TYPE string.

    IF selection_table IS NOT INITIAL AND variant IS INITIAL AND user IS INITIAL.

      SUBMIT (report)
            VIA JOB jobname NUMBER jobcount
            WITH SELECTION-TABLE selection_table
            TO SAP-SPOOL
            WITHOUT SPOOL DYNPRO
            ARCHIVE PARAMETERS arcparams
            SPOOL PARAMETERS priparams
            AND RETURN.
      subrc = sy-subrc.

    ELSEIF variant IS INITIAL.

      SUBMIT (report)
            VIA JOB jobname NUMBER jobcount
            USER user
            WITH SELECTION-TABLE selection_table
            TO SAP-SPOOL
            WITHOUT SPOOL DYNPRO
            ARCHIVE PARAMETERS arcparams
            SPOOL PARAMETERS priparams
            AND RETURN.
      subrc = sy-subrc.

      "'Error with free selections'  "#EC NOTEXT
      "'Error with free selections / static variant'  "#EC NOTEXT
    ELSEIF user IS INITIAL.

      SUBMIT (report)
            VIA JOB jobname NUMBER jobcount
            USING SELECTION-SET variant
            WITH SELECTION-TABLE selection_table
            TO SAP-SPOOL
            WITHOUT SPOOL DYNPRO
            ARCHIVE PARAMETERS arcparams
            SPOOL PARAMETERS priparams
            AND RETURN.
      subrc = sy-subrc.

    ELSE.

      SUBMIT (report)
            VIA JOB jobname NUMBER jobcount USER user
            USING SELECTION-SET variant
            WITH SELECTION-TABLE selection_table
            TO SAP-SPOOL
            WITHOUT SPOOL DYNPRO
            ARCHIVE PARAMETERS arcparams
            SPOOL PARAMETERS priparams
            AND RETURN.
      subrc = sy-subrc.

    ENDIF.

  ENDMETHOD.


  METHOD zif_job_llop~job_open.

    CONSTANTS: this_procedure TYPE symsgv VALUE 'JOB_OPEN' ##NO_TEXT.
    DATA: dummy TYPE string.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = jobname
        jobclass         = jobclass
        check_jobclass   = check_jobclass
      IMPORTING
        jobcount         = jobcount
        info             = info
      CHANGING
        ret              = ret
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    subrc = sy-subrc.

  ENDMETHOD.


  METHOD zif_job_llop~job_close.
    CONSTANTS: this_procedure TYPE symsgv VALUE 'JOB_CLOSE' ##NO_TEXT.
    DATA: "job_was_released TYPE btch0000-char1,
          "ret              TYPE i,
          dummy            TYPE string.
    "dont_release     TYPE btch0000-char1.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount                    = jobcount
        jobname                     = jobname
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

    subrc = sy-subrc.

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


  METHOD zif_job_llop~job_submit.

    DATA: dummy TYPE string.

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
        jobcount                    = jobcount
        jobname                     = jobname
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

    subrc = sy-subrc.

  ENDMETHOD.


ENDCLASS.
