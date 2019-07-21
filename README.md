# joboo
Background Job OO API

- Under construction - Not released -

Class ZCL_JOB to easily create and handle background jobs.

Demo code:

    TRY.
      DATA(job1) = NEW zcl_job( 'JOB1' ).
      job1->add_step_abap( report = 'Z_TEST' variant = 'V1' ).
      DATA(job2) = NEW zcl_job( 'JOB2' ).
      job2->add_step_abap( report = 'Z_TEST' variant = 'V1' ).
      DATA(job3) = NEW zcl_job( 'JOB3' ).
      job3->add_step_abap( report = 'Z_TEST' variant = 'V0' ).
      DATA(job4) = NEW zcl_job( 'JOB4' ).
      job4->add_step_abap( report = 'Z_TEST' variant = 'V1' ).
      DATA(job5) = NEW zcl_job( 'JOB5' ).
      job5->add_step_abap( report = 'Z_TEST' variant = 'V0' ).

      job5->schedule_after_job( job4 ).
      job4->schedule_after_job( job3 ).
      job3->schedule_after_job( job2 ).
      job2->schedule_after_job( job1 ).
      job1->schedule_at( date = '20171122' time = '020000' ).

    CATCH lcx_job INTO DATA(lx_job).
      MESSAGE lx_job TYPE 'I'.
    ENDTRY.
