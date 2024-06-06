*&---------------------------------------------------------------------*
*& Report ZZDD_SOFT_COMP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzdd_soft_comp.

TABLES: tdevc.

SELECT-OPTIONS: devclass FOR tdevc-devclass.
SELECT-OPTIONS: pdevclas FOR tdevc-pdevclass NO INTERVALS NO-EXTENSION.
SELECT-OPTIONS: dlvunit FOR tdevc-dlvunit NO INTERVALS NO-EXTENSION.
SELECT-OPTIONS: parentcl FOR tdevc-parentcl NO INTERVALS NO-EXTENSION.


PARAMETERS:
  test   RADIOBUTTON GROUP r1,
  commit RADIOBUTTON GROUP r1.


START-OF-SELECTION.
  CHECK 1 = 1.
* select
  SELECT *
    FROM tdevc
    INTO TABLE @DATA(lt_tdevc)
   WHERE devclass IN @devclass
     AND pdevclass IN @pdevclas
     AND dlvunit IN @dlvunit
     AND parentcl IN @parentcl.

  LOOP AT lt_tdevc ASSIGNING FIELD-SYMBOL(<tdevc>).

*    CLEAR:
*       <tdevc>-pdevclass,
*       <tdevc>-component,
*       <tdevc>-dlvunit.

    <tdevc>-dlvunit = 'RIVER'.
*    <tdevc>-parentcl = '/VIZIYA/MAIN'.

  ENDLOOP.

  UPDATE tdevc FROM TABLE lt_tdevc.


  IF commit = abap_true.
    COMMIT WORK AND WAIT.
    MESSAGE 'Commit Executed!' TYPE 'I'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE 'Rollback Executed!' TYPE 'S'.
  ENDIF.


*  data(lo_list) = new zcl_list3(  ).
*  lo_list->display(
*    EXPORTING
*      it_data         =   lt_tdevc
*      iv_struct_name  =   'TDEVC' ).


END-OF-SELECTION.
