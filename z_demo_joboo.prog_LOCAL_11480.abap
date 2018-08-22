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
REPORT.


* joboo2
CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
    CLASS-METHODS simple_immediately          RAISING zcx_job.
    CLASS-METHODS two_steps_immediately       RAISING zcx_job.
    CLASS-METHODS external_command_immediately RAISING zcx_job.
    CLASS-METHODS external_program_immediately RAISING zcx_job.
    CLASS-METHODS start_in_10_seconds         RAISING zcx_job.
    CLASS-METHODS start_daily_at_10_am        RAISING zcx_job.
    CLASS-METHODS start_daily_except_holidays RAISING zcx_job.
    CLASS-METHODS start_monthly_3rd_workday   RAISING zcx_job.
  PRIVATE SECTION.
    CLASS-DATA job TYPE REF TO zcl_job.
    CLASS-DATA lx_job TYPE REF TO zcx_job.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    TRY.
*        simple_immediately( ).
*        two_steps_immediately( ).
*        external_command_immediately( ).
        external_program_immediately( ).
        start_in_10_seconds( ).
        start_daily_at_10_am( ).
        start_daily_except_holidays( ).
        start_monthly_3rd_workday( ).
      CATCH zcx_job INTO lx_job.
        MESSAGE lx_job TYPE 'I'.
    ENDTRY.
  ENDMETHOD.
  METHOD simple_immediately.
    zcl_job=>new( name = 'one step' )->add_step_abap( report = sy-repid )->start_immediately( ).
  ENDMETHOD.
  METHOD two_steps_immediately.
    zcl_job=>new( name = 'two steps' )->add_step_abap( report = sy-repid )->add_step_abap( report = sy-repid )->start_immediately( ).
  ENDMETHOD.
  METHOD external_command_immediately.
    zcl_job=>new( name = 'SM49' )->add_step_external_command( command = 'ZDIR' )->start_immediately( ).
  ENDMETHOD.
  METHOD external_program_immediately.
    zcl_job=>new( name = 'OS command without SM49' )->add_step_external_program( program = 'cmd /c dir' wait_for_termination = abap_true )->start_immediately( ).
  ENDMETHOD.
  METHOD start_in_10_seconds.
    zcl_job=>new( name = 'OS command without SM49' ).
  ENDMETHOD.
  METHOD start_daily_at_10_am.
  ENDMETHOD.
  METHOD start_daily_except_holidays.
  ENDMETHOD.
  METHOD start_monthly_3rd_workday  .
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  CHECK sy-batch = abap_false.
  lcl_app=>main( ).
