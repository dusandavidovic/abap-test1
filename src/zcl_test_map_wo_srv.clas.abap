CLASS zcl_test_map_wo_srv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS validate
      IMPORTING
        !input  TYPE data
      EXPORTING
        !output TYPE data .
protected section.

  types:
    BEGIN OF ty_s_wo_data,
        hdr    TYPE REF TO data,
        t_oper TYPE REF TO data,
      END OF ty_s_wo_data .

  data MP_OUTPUT type ref to DATA .
  data MP_INPUT type ref to DATA .

  methods IS_WO_SRV
    importing
      !INPUT type DATA
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods MAPPER
    returning
      value(RV_CODE) type SYSUBRC .
  methods MANUAL
    returning
      value(RV_CODE) type SYSUBRC .
  methods SET_DATA
    importing
      !IS_INPUT type DATA
      !IS_OUTPUT type DATA
    returning
      value(RV_CODE) type SYSUBRC .
  methods SET_INIT
    returning
      value(RV_CODE) type SYSUBRC .
private section.

  data MO_MAPPER type ref to /RIVER/IF_BASE_MAPPER .
  data MO_IN_DESCR type ref to CL_ABAP_TYPEDESCR .
  data MO_OUT_DESCR type ref to CL_ABAP_TYPEDESCR .
  data MO_MSG type ref to /RIVER/IF_MSG .
  data MS_IN type TY_S_WO_DATA .
  data MS_OUT type TY_S_WO_DATA .

  methods MOVE_T_OPER
    importing
      !IT_IN type STANDARD TABLE
    exporting
      !ET_OUT type STANDARD TABLE .
  methods SPLIT_STRUCTURE
    importing
      !INPUT type DATA
    returning
      value(RS_SPLIT) type TY_S_WO_DATA .
ENDCLASS.



CLASS ZCL_TEST_MAP_WO_SRV IMPLEMENTATION.


  METHOD is_wo_srv.
    rv_result = abap_false.
    TRY .
        DATA: name TYPE fieldname VALUE 'ORDERID'.
        ASSIGN COMPONENT name OF STRUCTURE input TO FIELD-SYMBOL(<ff>).
        IF sy-subrc = 0.
          rv_result = abap_true.
        ENDIF.
      CATCH cx_root INTO DATA(lx_excpt) ##CATCH_ALL.
        DATA(text) = lx_excpt->get_text( ).
    ENDTRY.

  ENDMETHOD.


METHOD manual.

  TRY .
      FIELD-SYMBOLS:
        <in_str>  TYPE any,
        <out_str> TYPE any,
        <in_tab>  TYPE STANDARD TABLE,
        <out_tab> TYPE STANDARD TABLE.

      ASSIGN ms_in-t_oper->* TO <in_tab>.
      IF <in_tab> IS ASSIGNED.
        ASSIGN ms_out-t_oper->* TO <out_tab>.
        IF <out_tab> IS ASSIGNED.

            move_t_oper(
              EXPORTING it_in  = <in_tab>
              IMPORTING et_out = <out_tab> ).

        ENDIF.
      ENDIF.

    CATCH cx_root INTO DATA(lx_excpt) ##CATCH_ALL.
      mo_msg->add_exception_msg( lx_excpt ).
  ENDTRY.
ENDMETHOD.


METHOD mapper.
* mapper by using class
  TRY .
      FIELD-SYMBOLS:
        <in_str>  TYPE any,
        <out_str> TYPE any,
        <in_tab>  TYPE standard TABLE,
        <out_tab> TYPE standard TABLE.

      ASSIGN ms_in-hdr->* TO <in_str>.
      IF <in_str> IS ASSIGNED.
        ASSIGN ms_out-hdr->* TO <out_str>.
        IF <out_str> IS ASSIGNED.
          mo_mapper->str_by_name( EXPORTING is_in   = <in_str>
                                  IMPORTING es_out  = <out_str> ).
        ENDIF.
      ENDIF.

      ASSIGN ms_in-t_oper->* TO <in_tab>.
      IF <in_tab> IS ASSIGNED.
        ASSIGN ms_out-t_oper->* TO <out_tab>.
        IF <out_tab> IS ASSIGNED.
          mo_mapper->table_by_name( EXPORTING it_in  = <in_tab>
                                    IMPORTING et_out = <out_tab> ).
        ENDIF.
      ENDIF.

    CATCH cx_root INTO DATA(lx_excpt) ##CATCH_ALL.
      mo_msg->add_exception_msg( lx_excpt ).
  ENDTRY.

ENDMETHOD.


METHOD move_t_oper.

  DATA: iref TYPE REF TO data,
        oref TYPE REF TO data.
  CREATE DATA iref LIKE LINE OF it_in.
  ASSIGN iref->* TO FIELD-SYMBOL(<istr>).
  CREATE DATA oref LIKE LINE OF et_out.
  ASSIGN oref->* TO FIELD-SYMBOL(<ostr>).
  LOOP AT it_in ASSIGNING <istr> .
    MOVE-CORRESPONDING <istr> TO <ostr>.
    APPEND <ostr> TO et_out.
  ENDLOOP.

ENDMETHOD.


  METHOD set_data.
    TRY .
* parsing data...
        mo_in_descr = cl_abap_typedescr=>describe_by_data( is_input ).
        mo_out_descr = cl_abap_typedescr=>describe_by_data( is_output ).
* check if it contains TO_OPERATION field, if not exit
        IF mo_in_descr->kind = cl_abap_typedescr=>kind_struct.
          DATA(lo_str_descr) = CAST cl_abap_structdescr( mo_in_descr ).
          READ TABLE lo_str_descr->components WITH KEY name = 'TO_OPERATION'
                 TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            rv_code = 1.
            RETURN.
          ENDIF.
        ELSE.
          rv_code = 1.
          RETURN.
        ENDIF.

* split header from to_xxx
        ms_in = split_structure( is_input ).
        ms_out = split_structure( is_output ).

      CATCH cx_root INTO DATA(lx_excpt) ##CATCH_ALL.
        mo_msg->add_exception_msg( lx_excpt ).
    ENDTRY.
    rv_code = mo_msg->get_rc( ).
  ENDMETHOD.


  METHOD set_init.

    mo_msg = /river/cl_msg=>get_instance( ).
    TRY .
        DATA(lo_factory) = /river/cl_factory=>get_instance( ).
        mo_mapper = CAST #( lo_factory->create( iv_fact_type = /river/if_cfg=>c-fact_type-mapper ) ).
      CATCH cx_root INTO DATA(lx_excpt) ##CATCH_ALL.
        mo_msg->add_exception_msg( lx_excpt ).
    ENDTRY.
    rv_code = mo_msg->get_rc( ).
  ENDMETHOD.


METHOD split_structure.
  TRY .
      DATA(lo_str_descr) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( p_data = input ) ).
      DATA:
        ls_comp TYPE abap_componentdescr,
        lt_comp TYPE abap_component_tab.
      LOOP AT lo_str_descr->components ASSIGNING FIELD-SYMBOL(<comp>) .
        IF <comp>-name = 'TO_OPERATION'.
          ASSIGN COMPONENT <comp>-name OF STRUCTURE input TO FIELD-SYMBOL(<ff>).
          IF sy-subrc = 0.
*            rs_split-t_oper = REF #( <ff> ).
            /river/cl_data_svc=>copy_data_to_ref(
                        EXPORTING i_data  = <ff>
                        CHANGING  cp_data = rs_split-t_oper ).
          ENDIF.
        ELSE.
          ls_comp-name = <comp>-name.
          ASSIGN COMPONENT <comp>-name OF STRUCTURE input TO <ff>.
          IF sy-subrc = 0.
            ls_comp-type = CAST cl_abap_datadescr( cl_abap_datadescr=>describe_by_data( p_data = <ff> ) ).
            INSERT ls_comp INTO TABLE lt_comp.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF lines( lt_comp ) > 0.
        DATA(lo_hdr_descr) = cl_abap_structdescr=>get( p_components = lt_comp ).
        CREATE DATA rs_split-hdr TYPE HANDLE lo_hdr_descr.
        ASSIGN rs_split-hdr->* TO FIELD-SYMBOL(<hdr>).
        IF <hdr> IS ASSIGNED.
          MOVE-CORRESPONDING input TO <hdr>.
        ENDIF.
      ENDIF.

    CATCH cx_root INTO DATA(lx_excpt) ##CATCH_ALL.
      mo_msg->add_exception_msg( lx_excpt ).
  ENDTRY.

ENDMETHOD.


  METHOD validate.
* Note:
* This class is created to test move-corresponding error in /IWBEP/CL_RT_UTILITY_750
* It is invoked from method enhancement, ZZDD_TEST_MAP_WO_SRV, which is now deleted!

    IF is_wo_srv( input ) = abap_true.
      CHECK set_init( ) = 0.
      CHECK set_data(  is_input  = input
                       is_output = output ) = 0.
      CHECK mapper( ) = 0.
      CHECK manual( ) = 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
