*&---------------------------------------------------------------------*
*& Report ZZDD_IMPORT_EXPORT_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzdd_import_export_test.

PARAMETERS: pword TYPE text30.
PARAMETERS: pnumber TYPE i.

DATA:
  go_data_svc TYPE REF TO /river/cl_data_svc.

START-OF-SELECTION.

  go_data_svc = /river/cl_data_svc=>get_instance( ).
*  DATA(go_data_svc) = /river/cl_data_svc=>get_instance( ).

  PERFORM export.




END-OF-SELECTION.

FORM export.
  go_data_svc->set(
    EXPORTING
      iv_id            = 'ZZDD_WORD'
      i_data           = pword
*      iv_mem_type      = iv_mem_type
*      iv_transactional = iv_transactional
  ).

  go_data_svc->set(
    EXPORTING
      iv_id            = 'ZZDD_NUMBER'
      i_data           = pnumber
*      iv_mem_type      = iv_mem_type
*      iv_transactional = iv_transactional
  ).


  go_data_svc->set(
    EXPORTING
      iv_id            = 'ZZDD_WORD_EXP'
      i_data           = pword
      iv_mem_type      = go_data_svc->memory-import ).

  go_data_svc->set(
    EXPORTING
      iv_id            = 'ZZDD_NUMBER_IMP'
      i_data           = pnumber
      iv_mem_type      = go_data_svc->memory-import ).




ENDFORM.
