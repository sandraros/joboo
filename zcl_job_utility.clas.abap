********************************************************************************
*  MIT License
*
*  Copyright (c) 2018 sandraros
*
*  Permission is hereby granted, free of charge, to any person obtaining a copy
*  of this software and associated documentation files (the "Software"), to deal
*  in the Software without restriction, including without limitation the rights
*  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*  copies of the Software, and to permit persons to whom the Software is
*  furnished to do so, subject to the following conditions:
*
*  The above copyright notice and this permission notice shall be included in all
*  copies or substantial portions of the Software.
*
*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*  SOFTWARE.
********************************************************************************
"! <p class="shorttext synchronized" lang="en">Job utilities</p>
CLASS zcl_job_utility DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_ut_job TYPE TABLE OF REF TO zif_job .

    CLASS-METHODS wait_jobs
      IMPORTING
        !jobs  TYPE ty_ut_job OPTIONAL
        !delay TYPE numeric DEFAULT 10
          PREFERRED PARAMETER jobs
      RAISING
        zcx_job .
ENDCLASS.



CLASS zcl_job_utility IMPLEMENTATION.


  METHOD wait_jobs.

    DATA: lo_job TYPE REF TO zif_job.

    DO.

      LOOP AT jobs INTO lo_job
            WHERE table_line IS BOUND.

        CASE lo_job->get_state( ).
          WHEN lo_job->state-finished
            OR lo_job->state-aborted.
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
