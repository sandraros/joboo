"! <p class="shorttext synchronized" lang="en">Background job exception</p>
CLASS zcx_job DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    DATA bapiret2 TYPE bapiret2 READ-ONLY .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !bapiret2 TYPE bapiret2 OPTIONAL .
    CLASS-METHODS raise
      IMPORTING
        !bapiret2 TYPE bapiret2
      RAISING
        zcx_job .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_JOB IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->bapiret2 = bapiret2 .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD raise.
    DATA: ls_t100_key TYPE scx_t100key .

    ls_t100_key-msgno = bapiret2-number.
    ls_t100_key-msgid = bapiret2-id.
    ls_t100_key-attr1 = 'BAPIRET2-MESSAGE_V1' ##NO_TEXT.
    ls_t100_key-attr2 = 'BAPIRET2-MESSAGE_V2' ##NO_TEXT.
    ls_t100_key-attr3 = 'BAPIRET2-MESSAGE_V3' ##NO_TEXT.
    ls_t100_key-attr4 = 'BAPIRET2-MESSAGE_V4' ##NO_TEXT.

    RAISE EXCEPTION TYPE zcx_job
      EXPORTING
        textid   = ls_t100_key
        bapiret2 = bapiret2.
  ENDMETHOD.
ENDCLASS.
