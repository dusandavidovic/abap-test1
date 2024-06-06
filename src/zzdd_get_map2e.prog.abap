*&---------------------------------------------------------------------*
*& Report ZZDD_GET_MAP2E
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzdd_get_map2e.
* Selection screen

SELECTION-SCREEN BEGIN OF BLOCK bl_fcm WITH FRAME TITLE TEXT-fcm.
PARAMETERS: pfuncm TYPE rs38l-name  MATCHCODE OBJECT sfunc_modules MEMORY ID lib.
SELECTION-SCREEN END OF BLOCK bl_fcm.

* Local Processor Definition
CLASS lcl_proc DEFINITION.
  PUBLIC SECTION.
    METHODS:
      set_listbox IMPORTING iv_fldname TYPE clike,
      init,
      main.
  PROTECTED SECTION.
    DATA:
      mp_data        TYPE REF TO data,
      mo_transaction TYPE REF TO /river/if_transaction,
      mo_msg         TYPE REF TO /river/if_msg,
      mv_msg         TYPE string ##NEEDED.

    METHODS display IMPORTING it_list  TYPE ANY TABLE OPTIONAL
                              iv_title TYPE clike OPTIONAL.
    METHODS commit_or_rollback IMPORTING iv_commit TYPE flag.

  PRIVATE SECTION.
    TYPES: ty_t_include TYPE STANDARD TABLE OF progname WITH DEFAULT KEY.
    TYPES: ty_s_source TYPE rssource.
    TYPES: ty_t_source TYPE jjrssource_tab.
    DATA: mt_include TYPE ty_t_include.
    DATA: mv_main_program TYPE progname.

    TYPES:
      BEGIN OF ty_s_map_list,
        table_ext    TYPE  tabname,
        field_ext    TYPE  fieldname,
        descr_ext    TYPE string,
        namespace    TYPE  /river/namespace,
        column_order TYPE  int4,
        table_int    TYPE  tabname,
        field_int    TYPE  field_name,
        descr_int    TYPE string,
      END OF ty_s_map_list,
      ty_t_map_list TYPE STANDARD TABLE OF ty_s_map_list WITH DEFAULT KEY.

    DATA: mv_function_include TYPE rs38l-include.
    DATA: mv_function_group TYPE rs38l-area.
    DATA: mv_function_name TYPE rs38l-name.
    DATA: mt_map_list TYPE ty_t_map_list.


    METHODS get_main_program
      IMPORTING
        iv_function       TYPE rs38l_fnam
      RETURNING
        VALUE(rv_program) TYPE progname.
    METHODS get_includes
      IMPORTING
        iv_main_program   TYPE progname
      RETURNING
        VALUE(rt_include) TYPE lcl_proc=>ty_t_include.
    METHODS get_fmsource
      IMPORTING
        iv_function      TYPE rs38l-name
      RETURNING
        VALUE(rt_source) TYPE ty_t_source.
    METHODS set_attributes
      IMPORTING
        iv_function TYPE rs38l-name.
    METHODS parse_map2x
      IMPORTING
        it_src             TYPE string_table
      RETURNING
        VALUE(rt_map_list) TYPE ty_t_map_list.
    METHODS parse_map2x_line
      IMPORTING
        iv_line          TYPE string
        iv_substring     TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.
    METHODS get_nametab
      IMPORTING
        iv_structure    TYPE tabname
      RETURNING
        VALUE(r_result) TYPE pmget_name_tab.
    METHODS get_description
      IMPORTING
        iv_tabname      TYPE tabname
        iv_fieldname    TYPE fieldname
        it_nametab      TYPE pmget_name_tab
      RETURNING
        VALUE(r_result) TYPE string.
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

    set_attributes( pfuncm ).

    IF mo_msg->get_rc( ) = 0.
      DATA lt_src TYPE string_table.
      READ REPORT mv_function_include INTO lt_src.
      IF sy-subrc = 0.
        DATA(lt_ml) = parse_map2x( lt_src ).
      ENDIF.
    ENDIF.


*    mv_main_program = get_main_program( pfuncm ).
*    mt_include = get_includes( mv_main_program ).
*    DATA(lt_fmsource) = get_fmsource( pfuncm ).



    IF mo_msg->get_rc( ) > 0.
      mo_msg->show_messages( ).
    ENDIF.

    me->display( it_list = lt_ml ).

  ENDMETHOD.
  METHOD init.
    mo_msg ?= /river/cl_msg=>get_instance( ).
    mo_transaction ?= /river/cl_transaction_svc=>get_instance( mo_msg ).
  ENDMETHOD.
  METHOD display.
    DATA(lo_table) = NEW /river/cl_salv_list(  ).
    DATA: lv_title TYPE lvc_title.
    IF iv_title IS INITIAL.
      lv_title = |Records selected: { lines( it_list ) }|.
    ELSE.
      lv_title = iv_title.
    ENDIF.
    lo_table->set_defaults( iv_name  = lo_table->sc_default-title
                            iv_value = lv_title ).
    lo_table->set_defaults( iv_name  = lo_table->sc_default-gen_fcat
                            iv_value = abap_true ).
    lo_table->set_defaults( iv_name  = lo_table->sc_default-include_client
                            iv_value = abap_true ).
    IF lines( it_list ) > 0.
      lo_table->display(  it_list ).
    ELSE.
      MESSAGE s012(/river/filter).
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
  METHOD set_listbox.
    DATA:
      this_field TYPE text80,
      lt_values  TYPE vrm_values.
    this_field = iv_fldname.
    CASE this_field.
      WHEN 'PACTION'.
        lt_values = VALUE #( ( key = 'DLT' text = 'Delete records' ) ).
      WHEN OTHERS.
    ENDCASE.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = this_field
        values          = lt_values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD get_main_program.

    CALL FUNCTION 'GET_FUNCTION_MAIN_PROGRAM'
      EXPORTING
*       group_name     = group_name    " Function group, to which the function module belongs
        func_name      = iv_function    " Name of function module
      IMPORTING
        program_name   = rv_program    " Program name
      EXCEPTIONS
        prog_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      mo_msg->add_system_msg( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_includes.

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = iv_main_program
*       with_inactive_incls    = with_inactive_incls
*       with_reserved_includes = with_reserved_includes    " Internal
*       with_class_includes    = with_class_includes
      TABLES
        includetab   = rt_include
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      mo_msg->add_system_msg( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_fmsource.

*
*call function 'RPY_FUNCTIONMODULE_READ'
*  EXPORTING
*    functionname            = iv_function    " Name of the function module
**  IMPORTING
**    global_flag             = global_flag    " Global interface
**    remote_call             = remote_call    " Function module can be called via RFC
**    update_task             = update_task    " Function module running in update
**    short_text              = short_text    " Function module short text
**    function_pool           = function_pool
**    remote_basxml_supported = remote_basxml_supported
*  TABLES
*    import_parameter        = import_parameter    " Table of the import parameters
*    changing_parameter      = changing_parameter    " Table of CHANGING parameters
*    export_parameter        = export_parameter    " Table of the export parameters
*    tables_parameter        = tables_parameter    " Table of the tables
*    exception_list          = exception_list    " Table of exceptions
*    documentation           = documentation    " Documentation module and interface
*    source                  = source    " Function module source code
**  EXCEPTIONS
**    error_message           = 1
**    function_not_found      = 2
**    invalid_name            = 3
**    others                  = 4
*  .
*IF sy-subrc <> 0.
** MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*ENDIF.


  ENDMETHOD.


  METHOD set_attributes.

    CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
*      IMPORTING
*       functab             = pfuncm
*       namespace           = namespace    " Namespace
*       pname               = pname
      CHANGING
        funcname            = pfuncm
        group               = mv_function_group
        include             = mv_function_include
      EXCEPTIONS
        function_not_exists = 1
        include_not_exists  = 2
        group_not_exists    = 3
        no_selections       = 4
        no_function_include = 5
        OTHERS              = 6.
    IF sy-subrc <> 0.
      mo_msg->add_system_msg( ).
    ELSE.
      mv_function_name = pfuncm.
    ENDIF.


  ENDMETHOD.


  METHOD parse_map2x.
    DATA ls_map_list TYPE ty_s_map_list.
    DATA: lv_seq TYPE i.
    DATA lt_nametab_ext TYPE pmget_name_tab.
    DATA lt_nametab_int TYPE pmget_name_tab.

    LOOP AT it_src ASSIGNING FIELD-SYMBOL(<src>).
* <BAPI_STRUCTURE>|BAPI_NETWORK
      DATA(value) = parse_map2x_line( iv_line = <src> iv_substring = '<BAPI_STRUCTURE>|').
      ls_map_list-table_ext = COND #( WHEN value IS INITIAL THEN ls_map_list-table_ext ELSE value ).
      IF ls_map_list-table_ext IS NOT INITIAL.
        IF lines( lt_nametab_ext ) = 0.
          lt_nametab_ext = get_nametab( ls_map_list-table_ext ).
        ENDIF.
      ENDIF.
* <SAP_STRUCTURE>|CAUFVD
      value = parse_map2x_line( iv_line = <src> iv_substring = '<SAP_STRUCTURE>|').
      ls_map_list-table_int = COND #( WHEN value IS INITIAL THEN ls_map_list-table_int ELSE value ).
      IF ls_map_list-table_int IS NOT INITIAL.
        IF lines( lt_nametab_int ) = 0.
          lt_nametab_int = get_nametab( ls_map_list-table_int ).
        ENDIF.
      ENDIF.
      CHECK 1 = 1.

* <BAPI_FIELD>|SCHED_TYPE
      value = parse_map2x_line( iv_line = <src> iv_substring = '<BAPI_FIELD>|').
      ls_map_list-field_ext = COND #( WHEN value IS INITIAL THEN ls_map_list-field_ext ELSE value ).
      IF ls_map_list-field_ext IS NOT INITIAL.
        ls_map_list-descr_ext = get_description( iv_tabname = ls_map_list-table_ext
                                                 iv_fieldname = ls_map_list-field_ext
                                                 it_nametab = lt_nametab_ext ).
      ENDIF.
* <SAP_FIELD>|TERKZ
      value = parse_map2x_line( iv_line = <src> iv_substring = '<SAP_FIELD>|').
      ls_map_list-field_int = COND #( WHEN value IS INITIAL THEN ls_map_list-field_int ELSE value ).
      IF ls_map_list-field_int IS NOT INITIAL.
        ls_map_list-descr_int = get_description( iv_tabname = ls_map_list-table_int
                                                 iv_fieldname = ls_map_list-field_int
                                                 it_nametab = lt_nametab_int ).
      ENDIF.

* <ADD_FIELD>|
      IF  find( val = <src> sub = '<ADD_FIELD>|'  ) > 0.
*        lv_seq = lv_seq + 10.
*        ls_map_list-column_order = lv_seq.
        APPEND ls_map_list TO rt_map_list.
        CLEAR: ls_map_list-field_int, ls_map_list-descr_int, ls_map_list-field_ext, ls_map_list-descr_ext.
      ENDIF.

    ENDLOOP.

* combine with all fields from original structure
    lv_seq = 10.
    DATA(lt_map_list) = rt_map_list.
    CLEAR lt_map_list.
    SORT rt_map_list BY table_ext field_ext.
    LOOP AT lt_nametab_ext ASSIGNING FIELD-SYMBOL(<nametab_ext>).
      CLEAR ls_map_list.
      lv_seq = lv_seq + 10.

      READ TABLE rt_map_list ASSIGNING FIELD-SYMBOL(<list>) WITH KEY table_ext = <nametab_ext>-tabname
                                                                     field_ext = <nametab_ext>-fieldname
                                                                     BINARY SEARCH.
      IF sy-subrc = 0.
        ls_map_list = <list>.
      ELSE.
        ls_map_list-table_ext = <nametab_ext>-tabname.
        ls_map_list-field_ext = <nametab_ext>-fieldname.
        ls_map_list-descr_ext = <nametab_ext>-fieldtext.
      ENDIF.
      ls_map_list-column_order = <nametab_ext>-position.
      APPEND ls_map_list TO lt_map_list.
    ENDLOOP.
    rt_map_list = lt_map_list.
  ENDMETHOD.


  METHOD parse_map2x_line.
    CLEAR rv_result.
    DATA(off) = find( val = iv_line sub = iv_substring  ).
    IF off > 0.
      rv_result = substring_after( val = iv_line sub = iv_substring ).
    ENDIF.
  ENDMETHOD.


  METHOD get_nametab.

    CALL FUNCTION 'NAMETAB_GET'
      EXPORTING
*       langu               = langu    " Language
*       only                = only    " Choice: header (H) or Namtab (T) o
        tabname             = iv_structure
*    IMPORTING
*       header              = header    " returned table header (X030L)
*       rc                  = rc    " Various error statuses of table
      TABLES
        nametab             = r_result    " returned table structure
      EXCEPTIONS
        internal_error      = 1
        table_has_no_fields = 2
        table_not_activ     = 3
        no_texts_found      = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      mo_msg->add_system_msg( ).
    ELSE.
      SORT r_result BY tabname fieldname.
    ENDIF.

  ENDMETHOD.


  METHOD get_description.
    READ TABLE it_nametab ASSIGNING FIELD-SYMBOL(<nametab>) WITH KEY tabname = iv_tabname fieldname = iv_fieldname BINARY SEARCH.
    IF sy-subrc = 0.
      r_result = <nametab>-fieldtext.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
