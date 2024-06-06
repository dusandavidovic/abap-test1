*&---------------------------------------------------------------------*
*& Report ZZDD_TEST_COUNTER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzdd_test_counter.
* Selection screen
PARAMETERS: pcounter TYPE i,
            pdelay TYPE i.

* Local Processor Definition
CLASS lcl_proc DEFINITION.
  PUBLIC SECTION.
    METHODS:
      init,
      main.
  PROTECTED SECTION.
    METHODS execute.
    METHODS display
      IMPORTING it_list TYPE ANY TABLE OPTIONAL.
  PRIVATE SECTION.
    DATA:
      mp_data        TYPE REF TO data,
      mo_transaction TYPE REF TO /river/if_transaction,
      mo_msg         TYPE REF TO /river/if_msg,
      mv_msg         TYPE string ##NEEDED.

    METHODS:
      commit_or_rollback
        IMPORTING iv_commit TYPE flag.
ENDCLASS.

INITIALIZATION.
  DATA(go_proc) = NEW lcl_proc(  ) ##NEEDED.
  go_proc->init( ).

START-OF-SELECTION.
  go_proc->main( ).

END-OF-SELECTION.

* Local Processor Methods
CLASS lcl_proc IMPLEMENTATION.

  METHOD main.
*...
    me->execute( ).
*    me->display(  ).
  ENDMETHOD.
  METHOD init.
    mo_msg ?= /river/cl_msg=>get_instance( ).
    mo_transaction ?= /river/cl_transaction_svc=>get_instance( mo_msg ).

    pcounter = 3.
    pdelay = 20.
  ENDMETHOD.

  METHOD display.
    DATA(lo_table) = NEW /river/cl_salv_list(  ).
    IF lines( it_list ) > 0.
      lo_table->display(  it_list ).
    ELSE.
*    lo_table->display(  mt_list ).
    ENDIF.
  ENDMETHOD.
  METHOD commit_or_rollback.
    IF mo_msg->get_rc( ) IS INITIAL.
      IF iv_commit IS INITIAL.
        mo_transaction->rollback( ).
      ELSE.
        mo_transaction->commit( 'X' ).
      ENDIF.
    ELSE.
      mo_transaction->rollback( ).
    ENDIF.
  ENDMETHOD.

  METHOD execute.

     do pcounter times.
      wait Up to pdelay SECONDS.
      data(lv_elapsed) = sy-index * pdelay.
      MESSAGE s000(/river/job) WITH 'Counter:' sy-index ', Elapsed:' lv_elapsed.
     enddo.

  ENDMETHOD.
ENDCLASS.
