class ZCX_JOB_OPEN definition
  public
  inheriting from ZCX_JOB
  final
  create public .

public section.

  constants:
    begin of CANT_CREATE_JOB,
      msgid type symsgid value 'ZJOB',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANT_CREATE_JOB .
  constants:
    begin of INVALID_JOB_DATA,
      msgid type symsgid value 'ZJOB',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_JOB_DATA .
  constants:
    begin of JOBNAME_MISSING,
      msgid type symsgid value 'XM',
      msgno type symsgno value '046',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of JOBNAME_MISSING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !BAPIRET2 type BAPIRET2 optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_JOB_OPEN IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
BAPIRET2 = BAPIRET2
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.
ENDCLASS.
