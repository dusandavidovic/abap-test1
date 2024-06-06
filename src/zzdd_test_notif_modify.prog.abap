*&---------------------------------------------------------------------*
*& Report zzdd_get_function_params
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzdd_test_notif_modify.

* Selection screen
INCLUDE /river/sel_db ##INCL_OK.
PARAMETERS:
  pnotif TYPE qmnum,
  pstext TYPE string.

* Local Processor Definition
CLASS lcl_proc DEFINITION.
  PUBLIC SECTION.
    METHODS:
      init,
      main.
  PROTECTED SECTION.
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
        IMPORTING iv_commit TYPE flag,
      exec_bapi.

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

    exec_bapi(  ).

    IF mo_msg->get_rc(  ) > 0.
      mo_msg->show_messages( ).
    ELSE.
      commit_or_rollback(  pcommit ).
    ENDIF.
  ENDMETHOD.
  METHOD init.
    mo_msg ?= /river/cl_msg=>get_instance( ).
    mo_transaction ?= /river/cl_transaction_svc=>get_instance( mo_msg ).
  ENDMETHOD.

  METHOD display.
    DATA(lo_table) = NEW /river/cl_salv_list(  ).
    lo_table->set_defaults( iv_name  = lo_table->sc_default-gen_fcat
                            iv_value = abap_true ).
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



  METHOD exec_bapi.
    DATA number                     TYPE bapi2080_nothdre-notif_no.
    DATA notifheader                TYPE bapi2080_nothdri.
    DATA notifheader_x              TYPE bapi2080_nothdri_x.
    DATA no_buffer_refresh_on_error TYPE qm00-qkz.
    DATA maintactytype              TYPE ila.
    DATA notifheader_export         TYPE bapi2080_nothdre.
    DATA maintactytype_export       TYPE ila.
    DATA notifitem                  TYPE STANDARD TABLE OF bapi2080_notitemi.
    DATA notifitem_x                TYPE STANDARD TABLE OF bapi2080_notitemi_x.
    DATA notifcaus                  TYPE STANDARD TABLE OF bapi2080_notcausi.
    DATA notifcaus_x                TYPE STANDARD TABLE OF bapi2080_notcausi_x.
    DATA notifactv                  TYPE STANDARD TABLE OF bapi2080_notactvi.
    DATA notifactv_x                TYPE STANDARD TABLE OF bapi2080_notactvi_x.
    DATA notiftask                  TYPE STANDARD TABLE OF bapi2080_nottaski.
    DATA notiftask_x                TYPE STANDARD TABLE OF bapi2080_nottaski_x.
    DATA notifpartnr                TYPE STANDARD TABLE OF bapi2080_notpartnri.
    DATA notifpartnr_x              TYPE STANDARD TABLE OF bapi2080_notpartnri_x.
    DATA return                     TYPE STANDARD TABLE OF bapiret2.
    DATA extensionin                TYPE STANDARD TABLE OF bapiparex.
    DATA extensionout               TYPE STANDARD TABLE OF bapiparex.



    number = pnotif.
    notifheader-short_text = pstext.
    notifheader_x-short_text = abap_true.

    CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY'
      EXPORTING
        number                     = number    " Notification No
        notifheader                = notifheader    " BAPI Service Notification for Creation
        notifheader_x              = notifheader_x    " BAPI: Indicator for Notifcation Header
        no_buffer_refresh_on_error = no_buffer_refresh_on_error    " X and Blank
        maintactytype              = maintactytype
      IMPORTING
        notifheader_export         = notifheader_export    " BAPI Service Notification Header
        maintactytype_export       = maintactytype_export
      TABLES
        notifitem                  = notifitem    " Notification Item for Creation
        notifitem_x                = notifitem_x    " Change Indicator Changes of Notification Item
        notifcaus                  = notifcaus    " Notification Cause for Creation
        notifcaus_x                = notifcaus_x    " Change Indicator Changes of Notification Cause
        notifactv                  = notifactv    " Notification Activities for Creation
        notifactv_x                = notifactv_x    " Change Indicator Actions, Work Structure for Changes
        notiftask                  = notiftask    " Notification Task for Creation
        notiftask_x                = notiftask_x    " Change Indicator Change Notification Task
        notifpartnr                = notifpartnr    " Partner for Creation/Change
        notifpartnr_x              = notifpartnr_x    " Change Indicator Partner
        return                     = return.    " Return Parameters

    mo_msg->add_bapiret2_messages( return ).
    IF mo_msg->get_rc( ) = 0.
      CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
        EXPORTING
          number = number
*         together_with_order = together_with_order    " X and Blank
*         iv_refresh_complete = iv_refresh_complete    " X and Blank
*      IMPORTING
*         notifheader         = notifheader    " Notification Header Data
        TABLES
          return = return.    " Message Type: S = Success, E = Error, W = Warning, I = Infor
    ENDIF.
  ENDMETHOD.

ENDCLASS.
