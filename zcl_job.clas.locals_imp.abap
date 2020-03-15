*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
include RSXMIBAPI_MSG.

class lcl_LOW_LEVEL_OPS definition.

  public section.

  INTERFACES zif_job_llop.

    CLASS-METHODS convert_sy_to_bapiret2
      RETURNING
        VALUE(bapiret2) TYPE bapiret2 .

    CLASS-METHODS check_ret_code
      IMPORTING
        ret          TYPE i
        this_routine TYPE csequence
      RAISING
        zcx_job .

endclass.

class lcl_LOW_LEVEL_OPS implementation.

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
      IF sy-subrc <> 0.
        MESSAGE e027(bt) WITH report INTO dummy. " Failed to create job step & (see system log)
        zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).
      ENDIF.

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
      IF sy-subrc <> 0.
        MESSAGE e027(bt) WITH report INTO dummy. " Failed to create job step & (see system log)
        zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).
      ENDIF.

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
      IF sy-subrc <> 0.
        MESSAGE e027(bt) WITH report INTO dummy. " Failed to create job step & (see system log)
        zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).
      ENDIF.

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
      IF sy-subrc <> 0.
        MESSAGE e027(bt) WITH report INTO dummy. " Failed to create job step & (see system log)
        zcx_job=>raise( bapiret2 = convert_sy_to_bapiret2( ) ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_job_llop~job_open.

    CONSTANTS: this_routine TYPE symsgv VALUE 'JOB_OPEN' ##NO_TEXT.
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


  METHOD zif_job_llop~job_close.
    CONSTANTS: this_routine TYPE symsgv VALUE 'JOB_CLOSE' ##NO_TEXT.
    DATA: job_was_released TYPE btch0000-char1,
          ret              TYPE i,
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
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_job_lock_failed WITH space jobname jobcount.
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


  METHOD zif_job_llop~job_submit.
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

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          " should not happen, it should have been intercepted during
          " method process_print_archive_params.
          IF 0 = 1. MESSAGE e034(xm). ENDIF. " Internal problem (function &1)
          MESSAGE ID xmi_messages TYPE rs_c_error NUMBER msg_problem_detected WITH this_routine INTO dummy.
        WHEN 2.
          " TODO create a message for XPGFLAGS
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


endclass.
