class ZCX_JOB definition
  public
  inheriting from CX_STATIC_CHECK
  create private .

public section.

  interfaces IF_T100_MESSAGE .

  data BAPIRET2 type BAPIRET2 read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !BAPIRET2 type BAPIRET2 optional .
  class-methods RAISE
    importing
      !BAPIRET2 type BAPIRET2
    raising
      ZCX_JOB .
protected section.
private section.
ENDCLASS.



CLASS ZCX_JOB IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->BAPIRET2 = BAPIRET2 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.


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
