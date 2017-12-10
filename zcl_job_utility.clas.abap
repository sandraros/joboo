class ZCL_JOB_UTILITY definition
  public
  create public .

public section.

  types:
    ty_ut_job TYPE TABLE OF REF TO zif_job .

  class-methods WAIT_JOBS
    importing
      !JOBS type TY_UT_JOB optional
      !DELAY type NUMERIC default 10
    preferred parameter JOBS
    raising
      ZCX_JOB .
ENDCLASS.



CLASS ZCL_JOB_UTILITY IMPLEMENTATION.


  METHOD WAIT_JOBS.

    DATA: lo_job TYPE REF TO zif_job.

    DO.

      LOOP AT jobs INTO lo_job
            WHERE table_line IS BOUND.

        CASE lo_job->get_state( ).
          WHEN tybtc_finished OR tybtc_aborted.
            EXIT.
        ENDCASE.

      ENDLOOP.

      IF sy-subrc <> 0.
        " all jobs are finished or aborted.
        RETURN.
      ENDIF.

      WAIT UP TO delay SECONDS.

    ENDDO.


  ENDMETHOD.
ENDCLASS.
