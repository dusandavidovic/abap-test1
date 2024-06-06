*&---------------------------------------------------------------------*
*& Report ZZDD_LIST_USER_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzdd_list_user_test.
* Selection screen
SELECTION-SCREEN BEGIN OF BLOCK bl_sel WITH FRAME TITLE TEXT-bls.
PARAMETERS: uname    TYPE xubname,
            utype    TYPE xuustyp,
            ugroup   TYPE suid_st_node_logondata-class,
            valid_on TYPE dats.
SELECTION-SCREEN END OF BLOCK bl_sel.
SELECTION-SCREEN BEGIN OF BLOCK bl_opt WITH FRAME TITLE TEXT-blo.
PARAMETERS: pbapi  RADIOBUTTON GROUP r1,
            pcode  RADIOBUTTON GROUP r1,
            poldfm RADIOBUTTON GROUP r1.

PARAMETERS: pquery AS CHECKBOX.
PARAMETERS: pmaxrows TYPE i.
SELECTION-SCREEN END OF BLOCK bl_opt.
INCLUDE /river/sel_where ##INCL_OK.
INCLUDE /river/sel_order_by ##INCL_OK.
INCLUDE /river/sel_dsp.

* Local Processor Definition
CLASS lcl_proc DEFINITION.
  PUBLIC SECTION.
    METHODS:
      init,
      main.
  PROTECTED SECTION.
    METHODS display
      IMPORTING it_list TYPE ANY TABLE OPTIONAL.

    DATA: mt_cfg_map TYPE /river/if_cfg=>ty_t_cfg_map.
    DATA: mt_sel_params    TYPE suid_tt_search_node_fields.

    METHODS:
      execute_bapi
        RETURNING VALUE(rp_data) TYPE REF TO data,
      execute_bapi_detail
        IMPORTING userlist       TYPE suid_tt_bname
        RETURNING VALUE(rp_data) TYPE REF TO data,
      execute_code
        RETURNING VALUE(rp_data) TYPE REF TO data,
      execute_oldfm
        RETURNING VALUE(rp_data) TYPE REF TO data,
      set_sel_params,
      set_cfg
        IMPORTING id                TYPE string OPTIONAL
        RETURNING VALUE(rt_cfg_map) TYPE /river/if_cfg=>ty_t_cfg_map.

  PRIVATE SECTION.
    DATA:
      mp_data        TYPE REF TO data,
      mo_transaction TYPE REF TO /river/if_transaction,
      mo_msg         TYPE REF TO /river/if_msg,
      mv_msg         TYPE string ##NEEDED.

    METHODS:
      commit_or_rollback
        IMPORTING iv_commit TYPE flag,
      execute_code_detail
        IMPORTING
          it_bname       TYPE suid_tt_bname
        RETURNING
          VALUE(rp_data) TYPE REF TO data.
ENDCLASS.
* Local Processor Methods
CLASS lcl_proc IMPLEMENTATION.

  METHOD main.
*...
* prepare selection
    me->set_sel_params( ).
* execute
    CASE abap_true.
      WHEN pbapi.
        mp_data = execute_bapi( ).
      WHEN pcode.
        mp_data = execute_code( ).
      WHEN poldfm.
        mp_data = execute_oldfm( ).
    ENDCASE.

    IF pdsplist = abap_true.
      me->display(  ).
    ENDIF.
    IF pdspmsg = abap_true.
      mo_msg->show_messages( ).
    ENDIF.
  ENDMETHOD.
  METHOD execute_bapi.

    DATA with_username   TYPE bapiusmisc-with_name.
    DATA rows            TYPE bapiusmisc-bapirows.
    DATA userlist        TYPE STANDARD TABLE OF bapiusname.
    DATA return          TYPE STANDARD TABLE OF bapiret2.
    DATA selection_range TYPE STANDARD TABLE OF bapiussrge.

    selection_range = VALUE #( FOR ls_pp IN mt_sel_params
                               ( parameter = ls_pp-node_name field = ls_pp-field_name  sign = ls_pp-sign option = ls_pp-option low = ls_pp-low high = ls_pp-high ) ).


    CALL FUNCTION 'BAPI_USER_GETLIST'
      EXPORTING
        max_rows        = pmaxrows
        with_username   = pquery
      IMPORTING
        rows            = rows
      TABLES
        selection_range = selection_range
        userlist        = userlist
        return          = return.
    IF pquery = abap_true.
      CREATE DATA rp_data TYPE STANDARD TABLE OF bapiusname.
      FIELD-SYMBOLS: <ulist> TYPE STANDARD TABLE.
      ASSIGN rp_data->* TO <ulist>.
      IF sy-subrc = 0.
        <ulist> = userlist .
      ENDIF.
    ELSE.
*   prepare list of user Id's
      DATA(lt_bname) = VALUE suid_tt_bname( FOR ls_user IN userlist ( bname = ls_user-username ) ).
      rp_data = execute_bapi_detail( lt_bname ).
    ENDIF.

  ENDMETHOD.
  METHOD execute_bapi_detail.
    DATA:
      lo_mapper    TYPE REF TO /river/if_cfg_mapper,
      lp_user_info TYPE REF TO /river/user_info.

    mt_cfg_map = set_cfg( 'BAPI_MAPPER' ).
    DATA(lo_factory) = /river/cl_factory=>get_instance(  ).
    lo_mapper ?= lo_factory->create( /river/if_cfg=>c-fact_type-mapper ).
    lo_mapper->set_settings( iv_direction  = /river/if_cfg=>c-mapper-direction-int_to_ext
                             it_cfg_map    = mt_cfg_map  ).

    DATA logondata      TYPE bapilogond.
    DATA defaults       TYPE bapidefaul.
    DATA address        TYPE bapiaddr3.
    DATA company        TYPE bapiuscomp.
    DATA return         TYPE STANDARD TABLE OF bapiret2.

    FIELD-SYMBOLS: <utab> TYPE STANDARD TABLE.
    CREATE DATA rp_data TYPE /river/user_info_t.
    ASSIGN rp_data->* TO <utab>.

    LOOP AT userlist ASSIGNING FIELD-SYMBOL(<uline>).
      APPEND INITIAL LINE TO <utab> REFERENCE INTO lp_user_info.
      lp_user_info->user_name = <uline>-bname.

      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username      = <uline>-bname
          cache_results = ' '
        IMPORTING
          logondata     = logondata
          defaults      = defaults
          address       = address
          company       = company
        TABLES
          return        = return.

      IF return IS INITIAL.

        lo_mapper->set_settings( iv_table_name = 'DEFAULTS' ).
        lo_mapper->map_structure( EXPORTING is_data   = defaults
                                  CHANGING  cs_result = lp_user_info->* ).
        lo_mapper->set_settings( iv_table_name = 'LOGONDATA' ).
        lo_mapper->map_structure( EXPORTING is_data   = logondata
                                  CHANGING  cs_result = lp_user_info->* ).
        lo_mapper->set_settings( iv_table_name = 'ADDRESS' ).
        lo_mapper->map_structure( EXPORTING is_data   = address
                                  CHANGING  cs_result = lp_user_info->* ).
        lo_mapper->set_settings( iv_table_name = 'COMPANY' ).
        lo_mapper->map_structure( EXPORTING is_data   = company
                                  CHANGING  cs_result = lp_user_info->* ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD execute_code.

    DATA:
      ls_query_options TYPE suid_st_query_options,
      ls_return        TYPE bapiret2,
      lv_low           TYPE rsdsselop_,
      lv_high          TYPE rsdsselop_,
      lv_parameter     TYPE bapiusparm,
      lv_field         TYPE bapiusfld.

    SORT mt_cfg_map BY field_ext.
    LOOP AT mt_sel_params ASSIGNING FIELD-SYMBOL(<pp>).

      lv_parameter  = <pp>-node_name.
      lv_field      = <pp>-field_name.

      cl_suid_tools=>map_bapi_search_to_node(
        EXPORTING
          iv_parameter  = lv_parameter
          iv_field      = lv_field
        IMPORTING
          ev_node_name  = <pp>-node_name
          ev_field_name = <pp>-field_name
        CHANGING
          cv_low        = lv_low    " 'Generic' SELECT-OPTION for Dynamic Selections
          cv_high       = lv_high    " 'Generic' SELECT-OPTION for Dynamic Selections
          cs_return     = ls_return  ).  " Return parameters

    ENDLOOP.


* set selection parameters
    ls_query_options-maximum_rows = pmaxrows.
    ls_query_options-with_name = pquery.
    TRY.
        cl_identity_factory=>search(
          EXPORTING
            it_selection_parameters = mt_sel_params    " Table Type: Search Fields
            is_query_options        = ls_query_options    " Options for a Query
          IMPORTING
            eo_msg_buffer           = DATA(lo_msg_buffer)    " Read-Only Access
            es_query_info           = DATA(ls_query_info)    " Additional Information About the Search Result
            et_search_result        = DATA(lt_search_result) ).   " Search Result
      CATCH cx_suid_identity.    "
    ENDTRY.




    IF pquery = abap_true.
      CREATE DATA rp_data TYPE suid_tt_search_result.
      FIELD-SYMBOLS: <ulist> TYPE STANDARD TABLE.
      ASSIGN rp_data->* TO <ulist>.
      IF sy-subrc = 0.
        <ulist> = lt_search_result .
      ENDIF.
    ELSE.

*   prepare list of user Id's
* get details
      DATA(lt_bname) = VALUE suid_tt_bname( FOR ls_res IN lt_search_result ( bname = ls_res-username ) ).
      rp_data = execute_code_detail( lt_bname ).
    ENDIF.


  ENDMETHOD.

  METHOD set_sel_params.
* get parameters from program
    DATA: lt_sellist TYPE rsparams_tt.
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = sy-repid
      TABLES
        selection_table = lt_sellist[]    " Table with ranges structure that contains selections
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      mo_msg->add_system_msg( ).
    ENDIF.

    DATA: ls_sel_params TYPE suid_st_search_node_field.
    mt_cfg_map = me->set_cfg( 'PARMS' ).
    SORT mt_cfg_map BY field_ext.
    LOOP AT lt_sellist ASSIGNING FIELD-SYMBOL(<params>) WHERE low IS NOT INITIAL.
      IF <params>-selname = 'VALID_ON'.
        IF <params>-low <> '00000000'.
          ls_sel_params = VALUE #( node_name = 'LOGONDATA' field_name = 'GLTGV' sign = 'I' option = 'EQ' low = '00000000' ). APPEND ls_sel_params TO mt_sel_params.
          ls_sel_params = VALUE #( node_name = 'LOGONDATA' field_name = 'GLTGV' sign = 'I' option = 'LE' low = valid_on ). APPEND ls_sel_params TO mt_sel_params.

          ls_sel_params = VALUE #( node_name = 'LOGONDATA' field_name = 'GLTGB' sign = 'I' option = 'EQ' low = '00000000' ). APPEND ls_sel_params TO mt_sel_params.
          ls_sel_params = VALUE #( node_name = 'LOGONDATA' field_name = 'GLTGB' sign = 'I' option = 'GE' low = valid_on ). APPEND ls_sel_params TO mt_sel_params.
        ENDIF.
      ELSE.

        READ TABLE  mt_cfg_map ASSIGNING FIELD-SYMBOL(<cfg_map>) WITH KEY field_ext = <params>-selname BINARY SEARCH.
        IF sy-subrc = 0.
          ls_sel_params = VALUE #( node_name = <cfg_map>-table_int field_name =  <cfg_map>-field_int
                               sign = 'I' option = 'EQ' low = <params>-low ).
          IF ls_sel_params-node_name = 'USERNAME'. CLEAR ls_sel_params-field_name. ENDIF.
          APPEND ls_sel_params TO mt_sel_params.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_cfg.
    CASE id.
      WHEN 'PARMS'.
        rt_cfg_map = VALUE #(
                            (  table_ext = 'User' field_ext = 'UNAME' table_int = 'USERNAME' field_int = 'USERNAME' )
                            (  table_ext = 'User' field_ext = 'UTYPE' table_int = 'LOGONDATA' field_int = 'USTYP' )
                            (  table_ext = 'User' field_ext = 'UGROUP' table_int = 'LOGONDATA' field_int = 'CLASS' )
                            ).
      WHEN 'BAPI_MAPPER'.
        rt_cfg_map = VALUE #(
                            (  table_ext = 'User' field_ext = 'USER_NAME' table_int = 'USERNAME' field_int = 'USERNAME' )
                            (  table_ext = 'User' field_ext = 'FIRST_NAME' table_int = 'ADDRESS' field_int = 'FIRSTNAME' )
                            (  table_ext = 'User' field_ext = 'LAST_NAME' table_int = 'ADDRESS' field_int = 'LASTNAME' )
                            (  table_ext = 'User' field_ext = 'FULL_NAME' table_int = 'ADDRESS' field_int = 'FULLNAME' )
                            (  table_ext = 'User' field_ext = 'PERSON_NUMBER' table_int = 'ADDRESS' field_int = 'PERS_NO' )
                            (  table_ext = 'User' field_ext = 'ADDRESS_NUMBER' table_int = 'ADDRESS' field_int = 'ADDR_NO' )
                            (  table_ext = 'User' field_ext = 'DEPARTMENT' table_int = 'ADDRESS' field_int = 'DEPARTMENT' )
                            (  table_ext = 'User' field_ext = 'FUNCTION' table_int = 'ADDRESS' field_int = 'FUNCTION' )
                            (  table_ext = 'User' field_ext = 'E_MAIL' table_int = 'ADDRESS' field_int = 'E_MAIL' )
                            (  table_ext = 'User' field_ext = 'SAP_LANGUAGE' table_int = 'ADDRESS' field_int = 'LANGU' )
                            (  table_ext = 'User' field_ext = 'ISO_LANGUAGE' table_int = 'ADDRESS' field_int = 'LANGUP_ISO' )
                            (  table_ext = 'User' field_ext = 'NUMBER_FORMAT' table_int = 'DEFAULTS' field_int = 'DCPFM' )
                            (  table_ext = 'User' field_ext = 'DATE_FORMAT' table_int = 'DEFAULTS' field_int = 'DATFM' )
                            (  table_ext = 'User' field_ext = 'TIME_FORMAT' table_int = 'DEFAULTS' field_int = 'TIMEFM' )
                            (  table_ext = 'User' field_ext = 'DEFAULT_PRINTER' table_int = 'DEFAULTS' field_int = 'SPLD' )
                            (  table_ext = 'User' field_ext = 'VALID_FROM' table_int = 'LOGONDATA' field_int = 'GLTGV' )
                            (  table_ext = 'User' field_ext = 'VALID_TO' table_int = 'LOGONDATA' field_int = 'GLTGB' )
                            (  table_ext = 'User' field_ext = 'USER_TYPE' table_int = 'LOGONDATA' field_int = 'USTYP' )
                            (  table_ext = 'User' field_ext = 'USER_GROUP' table_int = 'LOGONDATA' field_int = 'CLASS' )
                            (  table_ext = 'User' field_ext = 'TIME_ZONE' table_int = 'DEFAULTS' field_int = 'TZONE' )
                            (  table_ext = 'User' field_ext = 'COMP_NAME' table_int = 'COMPANY' field_int = 'COMPANY' )
                            ).
      WHEN 'OLD_FM'.
        rt_cfg_map = VALUE #(
                            (  table_ext = 'OLD_FM' field_ext = 'ADDRESS_NUMBER' table_int = 'USR21' field_int = 'ADDRNUMBER' )
                            (  table_ext = 'OLD_FM' field_ext = 'DATE_FORMAT' table_int = 'USR01' field_int = 'DATFM' )
                            (  table_ext = 'OLD_FM' field_ext = 'DEFAULT_PRINTER' table_int = 'USR01' field_int = 'SPLD' )
                            (  table_ext = 'OLD_FM' field_ext = 'DEPARTMENT' table_int = 'ADCP' field_int = 'DEPARTMENT' )
                            (  table_ext = 'OLD_FM' field_ext = 'DISPLAY_NAME' table_int = 'ADRP' field_int = 'NAME_TEXT' )
                            (  table_ext = 'OLD_FM' field_ext = 'FIRST_NAME' table_int = 'ADRP' field_int = 'NAME_FIRST' )
                            (  table_ext = 'OLD_FM' field_ext = 'FUNCTION' table_int = 'ADCP' field_int = 'FUNCTION' )
                            (  table_ext = 'OLD_FM' field_ext = 'ISO_LANGUAGE' table_int = 'T002' field_int = 'LAISO' )
                            (  table_ext = 'OLD_FM' field_ext = 'LAST_NAME' table_int = 'ADRP' field_int = 'NAME_LAST' )
                            (  table_ext = 'OLD_FM' field_ext = 'NUMBER_FORMAT' table_int = 'USR01' field_int = 'DCPFM' )
                            (  table_ext = 'OLD_FM' field_ext = 'PERSON_NUMBER' table_int = 'USR21' field_int = 'PERSNUMBER' )
                            (  table_ext = 'OLD_FM' field_ext = 'SAP_LANGUAGE' table_int = 'USR01' field_int = 'LANGU' )
                            (  table_ext = 'OLD_FM' field_ext = 'TIME_ZONE' table_int = 'USR02' field_int = 'TZONE' )
                            (  table_ext = 'OLD_FM' field_ext = 'USER_GROUP' table_int = 'USR02' field_int = 'CLASS' )
                            (  table_ext = 'OLD_FM' field_ext = 'USER_NAME' table_int = 'USR21' field_int = 'BNAME' )
                            ).
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD init.
    mo_msg ?= /river/cl_msg=>get_instance( ).
    mo_transaction ?= /river/cl_transaction_svc=>get_instance( mo_msg ).
  ENDMETHOD.

  METHOD display.
    FIELD-SYMBOLS: <list> TYPE STANDARD TABLE.
    ASSIGN mp_data->* TO <list>.
    IF sy-subrc = 0.
      DATA(lo_table) = NEW /river/cl_salv_list(  ).
      IF lines( <list> ) > 0.
        lo_table->display(  <list> ).
      ELSE.
        MESSAGE i000(/river/common) WITH 'No data to display!'.
      ENDIF.
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

  METHOD execute_code_detail.

    DATA:
      lt_ustyp_domval TYPE if_dd_types=>ty_t_domvalues,
      lt_nodes        TYPE suid_tt_node.

*  user type domain values
    lt_ustyp_domval = cl_domain=>get_fixed_values( domain_name = 'XUUSTYP' ).

    TRY.
* 1. prefetch nodes
        lt_nodes = VALUE #( ( nodename = if_identity_definition=>gc_node_defaults )
                            ( nodename = if_identity_definition=>gc_node_logondata )
                            ( nodename = if_identity_definition=>gc_node_person_name )
                            ( nodename = if_identity_definition=>gc_node_person )
                            ( nodename = if_identity_definition=>gc_node_workplace )
                            ( nodename = if_identity_definition=>gc_node_organization )
                            ( nodename = if_identity_definition=>gc_node_email ) ).

        cl_identity_factory=>nodes_prefetch( EXPORTING it_bname          = it_bname
                                                       it_nodes_prefetch = lt_nodes ).

* 2. retrieve
        DATA:
          lo_msg_buffer TYPE REF TO   if_suid_msg_buffer,
          lt_node_root  TYPE          suid_tt_node_root.
        cl_identity_factory=>retrieve(
              EXPORTING it_bname      = it_bname
              IMPORTING et_node_root  = lt_node_root
                        eo_msg_buffer = lo_msg_buffer ).

      CATCH cx_suid_identity INTO DATA(lx_excpt).
    ENDTRY.

* prepare mapping configuration
    DATA:
      ls_address           TYPE bapiaddr3,
      ls_retrieve_commdata TYPE bup_s_commdata_read_api,
      lo_mapper            TYPE REF TO /river/if_cfg_mapper,
      lp_user_info         TYPE REF TO /river/user_info.

    mt_cfg_map = set_cfg( 'BAPI_MAPPER' ).
    DATA(lo_factory) = /river/cl_factory=>get_instance(  ).
    lo_mapper ?= lo_factory->create( /river/if_cfg=>c-fact_type-mapper ).
    lo_mapper->set_settings( iv_direction  = /river/if_cfg=>c-mapper-direction-int_to_ext
                             it_cfg_map    = mt_cfg_map  ).


    FIELD-SYMBOLS: <utab> TYPE STANDARD TABLE.
    CREATE DATA rp_data TYPE /river/user_info_t.
    ASSIGN rp_data->* TO <utab>.

* 3. for each user, get node
    LOOP AT lt_node_root ASSIGNING FIELD-SYMBOL(<node_root>).
      APPEND INITIAL LINE TO <utab> REFERENCE INTO lp_user_info.

      lp_user_info->user_name = <node_root>-bname.


      TRY.

          lo_mapper->set_settings( iv_table_name = 'LOGONDATA' ).
          <node_root>-idref->get_logondata(
            IMPORTING
              es_logondata            = DATA(ls_logondata)    " Structure Type: Logon Data for User
              eo_msg_buffer           = lo_msg_buffer  ).
          lo_mapper->map_structure( EXPORTING is_data   = ls_logondata
                                    CHANGING  cs_result = lp_user_info->* ).
*         valid today
          IF   ( ls_logondata-gltgv IS INITIAL   AND  ls_logondata-gltgb IS INITIAL  ) OR
               ( ls_logondata-gltgb GE sy-datum  AND  ls_logondata-gltgv LE sy-datum ) OR
               ( ls_logondata-gltgv LE sy-datum  AND  ls_logondata-gltgb IS INITIAL  ).
            lp_user_info->valid_today = abap_true.
          ENDIF .
*         logon language - only for current user
          IF <node_root>-bname = sy-uname.
            lp_user_info->logon_language = sy-langu.
            CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
              EXPORTING
                input  = sy-langu
              IMPORTING
                output = lp_user_info->logon_iso_language.
          ENDIF.

*         user type description
          READ TABLE lt_ustyp_domval ASSIGNING FIELD-SYMBOL(<domval>)
                     WITH KEY value = lp_user_info->user_type BINARY SEARCH.
          IF sy-subrc = 0.
            lp_user_info->user_type_descr = <domval>-description.
          ENDIF.

          lo_mapper->set_settings( iv_table_name = 'DEFAULTS' ).
          <node_root>-idref->get_defaults(
                                  IMPORTING eo_msg_buffer = lo_msg_buffer
                                            es_defaults   = DATA(ls_defaults)
                                            ev_kostl      = DATA(lv_kostl) ).
          lo_mapper->map_structure( EXPORTING is_data   = ls_defaults
                                              CHANGING  cs_result = lp_user_info->* ).


          <node_root>-idref->if_identity_person~get_person(
                          IMPORTING
                            es_person           = DATA(ls_person)
                            eo_msg_buffer           = lo_msg_buffer ).
          <node_root>-idref->if_identity_person~get_personname(
                IMPORTING
                  es_personname           = DATA(ls_person_name1)
                  eo_msg_buffer           = lo_msg_buffer ).
          lo_mapper->set_settings( iv_table_name = 'ADDRESS' ).
          <node_root>-idref->if_identity_address~get_personname(
                IMPORTING
                  es_personname           = DATA(ls_person_name)
                  eo_msg_buffer           = lo_msg_buffer ).


          <node_root>-idref->if_identity_address~get_workplacedata(
                IMPORTING
                  es_workplace            = DATA(ls_workplace)
                  eo_msg_buffer           = lo_msg_buffer ).

          <node_root>-idref->if_identity_address~get_organization(
            EXPORTING
              iv_get_location          = if_identity=>co_true
              iv_get_organization_name = if_identity=>co_true
            IMPORTING
              es_organization          = DATA(ls_organisation)
              eo_msg_buffer            = lo_msg_buffer
              es_location              = DATA(ls_location)
              es_organization_name     = DATA(ls_organisation_name) ).

          CALL METHOD cl_suid_tools=>map_address_nodes_to_bapi
            EXPORTING
              is_node_person_name = ls_person_name
              is_node_workplace   = ls_workplace
              is_node_location    = ls_location
*             is_node_organization      = ls_organisation
*             is_node_organization_name = ls_organisation_name
            IMPORTING
              et_return           = DATA(lt_return)
            CHANGING
              cs_bapiaddr3        = ls_address.

          CALL METHOD cl_suid_tools=>map_address_code_sap_to_iso
            EXPORTING
              iv_bname            = lp_user_info->user_name
              is_node_person_name = ls_person_name
              is_node_location    = ls_location
            IMPORTING
              et_messages         = DATA(lt_new_messages)
            CHANGING
              cs_bapiaddr3        = ls_address.

          lo_mapper->map_structure( EXPORTING is_data   = ls_address
                                    CHANGING  cs_result = lp_user_info->* ).
* company is not mapped for assignment. Instead it is mapped for filtering
          lp_user_info->comp_name = ls_address-name.

* Language determination: Default->Person->Company
          IF ls_defaults-langu IS NOT INITIAL.
            lp_user_info->sap_language = ls_defaults-langu.
            CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
              EXPORTING
                input  = ls_defaults-langu
              IMPORTING
                output = lp_user_info->logon_iso_language.
          ELSE.
            IF ls_address-langu_p IS NOT INITIAL.
              lp_user_info->sap_language = ls_address-langu_p.
              lp_user_info->iso_language = ls_address-langup_iso.
            ELSE.
              lp_user_info->sap_language = ls_address-langu.
              lp_user_info->iso_language = ls_address-langu_iso.
            ENDIF.
          ENDIF.

          ls_retrieve_commdata-email   = if_identity=>co_true.
          <node_root>-idref->if_identity_address~get_communication_data(
                                          EXPORTING  is_retrieve_commdata = ls_retrieve_commdata
                                          IMPORTING  et_email       = DATA(lt_email)
                                                     eo_msg_buffer  = lo_msg_buffer ).

*         return first email
          READ TABLE lt_email ASSIGNING FIELD-SYMBOL(<email>) INDEX 1.
          IF sy-subrc = 0.
            lp_user_info->e_mail = <email>-smtp_addr.
          ENDIF.



        CATCH cx_suid_identity INTO lx_excpt.
      ENDTRY.

    ENDLOOP.


  ENDMETHOD.
  METHOD execute_oldfm.

* prepare mapping
    mt_cfg_map =  set_cfg( 'OLD_FM' ).
    DATA ls_cfg_dpc TYPE  /river/if_cfg=>ty_s_cfg_dpc.
    ls_cfg_dpc-entity_name = 'OLD_FM'.
    DATA(lo_factory) = /river/cl_factory=>get_instance(  ).
    ls_cfg_dpc-map ?= lo_factory->create( /river/if_cfg=>c-fact_type-map ).
    ls_cfg_dpc-map->set_cfg_map_t( mt_cfg_map ).
* DPC helper
    DATA(lo_helper) =  CAST /river/cl_dpc_helper( lo_factory->create( /river/if_c=>cfg_type-dpc_helper ) ).
    lo_helper->set_cfg_dpc_s( ls_cfg_dpc ).
    lo_helper->set_740_sql( abap_true ).
* set sql options
    DATA: ls_sql_options TYPE /river/sql_options.
    ls_sql_options-t_select = lo_helper->get_sql_select( ).
*  ls_sql_options-orderby = lo_helper->get_sql_orderby( is_cfg_dpc = ls_cfg_dpc it_orderby = io_tech_request_context->get_orderby( ) ).
*  DATA(lt_source_keys) = io_tech_request_context->get_source_keys( ).


    ls_sql_options-t_where = lo_helper->get_sql_where(  " it_source_keys  = lt_source_keys
                                                  iv_where_clause = where ).
*                                                    io_mgw_filter   = io_tech_request_context->get_filter( ) ).


    DATA lv_return_code   TYPE i.
    DATA lt_query_params  TYPE zzv_query_params_t.
    DATA lt_conditions    TYPE zzv_conditions_t.
    DATA ls_conditions    TYPE zzv_conditions_s.
    DATA lt_sort_order    TYPE zzv_abap_sort_order_t.
    DATA lt_query_results TYPE zzv_user_info_t.
    DATA lt_messages      TYPE bapiret2_t.
    LOOP AT ls_sql_options-t_where ASSIGNING FIELD-SYMBOL(<line>).
      ls_conditions = <line>.
      APPEND ls_conditions TO lt_conditions.
    ENDLOOP.

    CALL FUNCTION 'ZZV_USER_GET_LIST'
      EXPORTING
*       IV_APP           = IV_APP
*       IV_OBJ_TYPE_LIST = 'LIST_USER'
*       IV_OBJ_TYPE_MAP  = 'LIST_USER'
        iv_max_rows      = pmaxrows
      IMPORTING
        ev_return_code   = lv_return_code
      TABLES
        it_query_params  = lt_query_params
        it_conditions    = lt_conditions
        it_sort_order    = lt_sort_order
        et_query_results = lt_query_results
        et_messages      = lt_messages.

     /RIVER/CL_DATA_SVC=>copy_data_to_ref(
       EXPORTING
         i_data  = lt_query_results
       CHANGING
         cp_data = rp_data ).

  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  DATA(go_proc) = NEW lcl_proc(  ) ##NEEDED.
  go_proc->init( ).
  valid_on = sy-datum.
  pmaxrows = 100.

START-OF-SELECTION.
  go_proc->main( ).

END-OF-SELECTION.
