*&---------------------------------------------------------------------*
*& Report ZZDD_TEST_NETWORK_MAINTAIN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzdd_test_network_maintain.

* Selection screen
INCLUDE /river/sel_db ##INCL_OK.

SELECTION-SCREEN BEGIN OF BLOCK bl_act WITH FRAME TITLE TEXT-act. "#EC SHAREOK
PARAMETERS:
  pcreate RADIOBUTTON GROUP rad1,
  pcrtact RADIOBUTTON GROUP rad1,
  pupdact RADIOBUTTON GROUP rad1,
  pupdate RADIOBUTTON GROUP rad1.
SELECTION-SCREEN SKIP.
PARAMETERS: psched  AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK bl_act.

SELECTION-SCREEN BEGIN OF BLOCK bl_sel WITH FRAME TITLE TEXT-sel. "#EC SHAREOK
PARAMETERS:
  pnetwork TYPE nw_aufnr,
  pnetact  TYPE cn_vornr,
  pstext   TYPE string LOWER CASE,
  pstart   TYPE dats.
SELECTION-SCREEN END OF BLOCK bl_sel.



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

    mo_msg->show_messages( ).
    commit_or_rollback(  pcommit ).
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


    CALL FUNCTION 'BAPI_PS_INITIALIZATION'.


    DATA:
      e_message_table             TYPE STANDARD TABLE OF    bapi_meth_message WITH DEFAULT KEY,
      i_method_project            TYPE STANDARD TABLE OF  bapi_method_project WITH DEFAULT KEY,
      i_network                   TYPE STANDARD TABLE OF  bapi_network WITH DEFAULT KEY,
      i_network_update            TYPE STANDARD TABLE OF    bapi_network_update WITH DEFAULT KEY,
      i_activity                  TYPE STANDARD TABLE OF    bapi_network_activity WITH DEFAULT KEY,
      i_activity_update           TYPE STANDARD TABLE OF    bapi_network_activity_up WITH DEFAULT KEY,
      i_relation                  TYPE STANDARD TABLE OF    bapi_network_relation WITH DEFAULT KEY,
      i_relation_update           TYPE STANDARD TABLE OF    bapi_network_relation_up WITH DEFAULT KEY,
      i_activity_element          TYPE STANDARD TABLE OF    bapi_act_element WITH DEFAULT KEY,
      i_activity_element_update   TYPE STANDARD TABLE OF    bapi_act_element_upd WITH DEFAULT KEY,
      i_activity_milestone        TYPE STANDARD TABLE OF    bapi_act_milestone WITH DEFAULT KEY,
      i_activity_milestone_update TYPE STANDARD TABLE OF    bapi_act_milestone_upd WITH DEFAULT KEY.

    DATA:
      lv_refnumber                 TYPE ifrefnum VALUE '000001',
      ls_return                    TYPE  bapireturn1,
      ls_bapiret2                  TYPE  bapiret2,
      ls_method_project            TYPE    bapi_method_project,
      ls_network                   TYPE    bapi_network,
      ls_network_update            TYPE    bapi_network_update,
      ls_activity                  TYPE    bapi_network_activity,
      ls_activity_update           TYPE    bapi_network_activity_up,
      ls_relation                  TYPE    bapi_network_relation,
      ls_relation_update           TYPE    bapi_network_relation_up,
      ls_message_table             TYPE    bapi_meth_message,
      ls_activity_element          TYPE    bapi_act_element,
      ls_activity_element_update   TYPE    bapi_act_element_upd,
      ls_activity_milestone        TYPE    bapi_act_milestone,
      ls_activity_milestone_update TYPE    bapi_act_milestone_upd.

**********************************************************************

** Method
    IF pcreate = abap_true.
      ls_method_project-objecttype = 'NETWORK'.
      ls_method_project-method = 'CREATE'.
      ls_method_project-refnumber = lv_refnumber.
      ls_method_project-objectkey = 'TRAINING0001'.
      APPEND ls_method_project TO i_method_project.
      CLEAR ls_method_project.

      ls_method_project-method = 'SAVE'.
      APPEND ls_method_project TO i_method_project.

** Network
      ls_network-network = 'TRAINING0001'.
      ls_network-network_type = 'PS05'.
      ls_network-short_text = 'ZZDD New Network'.
      ls_network-finish_date = '20241130'.
      ls_network-start_date = '20241101'.
      ls_network-profile = '1002'.
      ls_network-wbs_element = 'Z/022-1  -002'.

      APPEND ls_network TO i_network.

    ELSEIF pupdate = abap_true.

      ls_method_project-objecttype = 'NETWORK'.
      ls_method_project-method = 'UPDATE'.
      ls_method_project-refnumber = lv_refnumber.
      ls_method_project-objectkey = pnetwork.
      APPEND ls_method_project TO i_method_project.
      CLEAR ls_method_project.

      ls_method_project-method = 'SAVE'.
      APPEND ls_method_project TO i_method_project.

** Network
      ls_network-network = pnetwork.
*      ls_network-network_type = 'PS05'.
      ls_network-short_text = pstext.
      ls_network-start_date = pstart.
      IF pstart IS NOT INITIAL.
        ls_network-finish_date = pstart + 5.
      ENDIF.
      APPEND ls_network TO i_network.

      ls_network_update-network = pnetwork.
      IF pstext IS NOT INITIAL.
        ls_network_update-short_text = abap_true.
      ENDIF.
      IF pstart IS NOT INITIAL.
        ls_network_update-start_date = abap_true.
      ENDIF.
      IF ls_network-finish_date IS NOT INITIAL.
        ls_network_update-finish_date = abap_true.
      ENDIF.
      APPEND ls_network_update TO i_network_update.

    ELSEIF pcrtact = abap_true.
* Method
      DATA(lv_objectkey) = |{ pnetwork }{ pnetact }|.
      ls_method_project-objecttype = 'NETWORKACTIVITY'.
      ls_method_project-method = 'CREATE'.
      ls_method_project-refnumber = lv_refnumber.
      ls_method_project-objectkey = lv_objectkey.
      APPEND ls_method_project TO i_method_project.
      CLEAR ls_method_project.

      ls_method_project-method = 'SAVE'.
      APPEND ls_method_project TO i_method_project.

* Network
      ls_network-network = pnetwork.
      APPEND ls_network TO i_network.

* Activity
      ls_activity-network = pnetwork.
      ls_activity-activity = pnetact.
      ls_activity-control_key = 'PS01'.
      ls_activity-description = pstext.
      APPEND ls_activity TO i_activity.


    ELSEIF pupdact = abap_true.

    ENDIF.

    IF psched = abap_true.
      ls_method_project-objecttype = 'NETWORK'.
      ls_method_project-method = 'SCHEDULE'.
      ls_method_project-refnumber = lv_refnumber + 1.
      ls_method_project-objectkey = pnetwork.
      APPEND ls_method_project TO i_method_project.
      CLEAR ls_method_project.

      READ TABLE i_method_project WITH KEY method = 'SAVE' TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ls_method_project-method = 'SAVE'.
        APPEND ls_method_project TO i_method_project.
      ENDIF.
    ENDIF.

**********************************************************************

    CALL FUNCTION 'BAPI_NETWORK_MAINTAIN'
      IMPORTING
        return                      = ls_return
      TABLES
        i_method_project            = i_method_project
        i_network                   = i_network
        i_network_update            = i_network_update
        i_activity                  = i_activity
        i_activity_update           = i_activity_update
        i_relation                  = i_relation
        i_relation_update           = i_relation_update
        e_message_table             = e_message_table
        i_activity_element          = i_activity_element
        i_activity_element_update   = i_activity_element_update
        i_activity_milestone        = i_activity_milestone
        i_activity_milestone_update = i_activity_milestone_update.

    ls_bapiret2 = CORRESPONDING #( ls_return ).
    mo_msg->add_bapiret2_message( is_bapiret2 = ls_bapiret2 ).


    LOOP AT e_message_table ASSIGNING FIELD-SYMBOL(<msg>).
      MESSAGE ID <msg>-message_id TYPE <msg>-message_type NUMBER <msg>-message_number INTO mv_msg.
      mo_msg->add_system_msg( ).
    ENDLOOP.


**********************************************************************
    DATA: lt_bapiret2 TYPE bapiret2_t.
    CALL FUNCTION 'BAPI_PS_PRECOMMIT'
      TABLES
        et_return = lt_bapiret2[].    " Return Parameter

    mo_msg->add_bapiret2_messages( it_bapiret2 = lt_bapiret2 ).


  ENDMETHOD.

ENDCLASS.
