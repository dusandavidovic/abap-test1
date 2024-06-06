REPORT  zzdd_get_object_info.
TABLES: tadir.

TYPES: BEGIN OF s_obji,
         obj_name TYPE sobj_name,
         devclass  TYPE devclass,
         ctext     TYPE as4text,
       END OF s_obji,
       t_obji TYPE STANDARD TABLE OF s_obji WITH DEFAULT KEY.
*----------------------------------------------------------------------*
*       CLASS lcl_list  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_list DEFINITION
              INHERITING FROM /river/cl_salv_list.

  PUBLIC SECTION.
    DATA: t_list TYPE t_obji.

  PROTECTED SECTION.
    METHODS: set_list_instance REDEFINITION.
    METHODS: data_setup REDEFINITION.
ENDCLASS.                    "lcl_list  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS lcl_list IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_list IMPLEMENTATION.
  METHOD set_list_instance.
    cl_salv_table=>factory( IMPORTING r_salv_table   = me->list
                            CHANGING  t_table        = me->t_list ).
  ENDMETHOD.                    "SET_LIST_INSTANCE

  METHOD data_setup.
    me->t_list = it_data.
  ENDMETHOD.                    "DATA_SETUP

ENDCLASS.                    "lcl_list IMPLEMENTATION



SELECTION-SCREEN SKIP.
*PARAMETERS: p_file TYPE string LOWER CASE MEMORY ID lfile.
*SELECTION-SCREEN SKIP.

SELECT-OPTIONS: s_obj FOR tadir-obj_name.

INITIALIZATION.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  CALL METHOD zcl_file=>open_file_dialog
*    RECEIVING
*      rv_filename = p_file.



START-OF-SELECTION.
*  DATA: lo_err TYPE REF TO zcx_file2,
*        lv_msg TYPE string,
*        lt_raw TYPE zcl_file2=>t_string.
*  TRY.
*      zcl_file2=>upload_ascii_file(
*        EXPORTING
*          iv_filename        = p_file
*        IMPORTING
*           et_data            = lt_raw ).
*    CATCH zcx_file2  INTO lo_err.
*  ENDTRY.

  DATA: lt_obji TYPE t_obji.
* get info
  SELECT t1~obj_name t1~devclass t2~ctext
    INTO TABLE lt_obji
    FROM tadir AS t1 JOIN
         tdevct AS t2
         on t1~devclass = t2~devclass
  WHERE t1~obj_name in s_obj
    and t2~spras = sy-langu.


  IF lt_obji IS NOT INITIAL.
    DATA: lo_list TYPE REF TO lcl_list,
          ls_la TYPE /river/cl_salv_base=>list_s_attr.
*
    ls_la-title = 'AAA Title'.
    ls_la-free_text = 'AAA Free text'.
*
    CREATE OBJECT lo_list.
    lo_list->set_list_attributes( is_list_attr = ls_la ).
    lo_list->display( lt_obji ).
  ENDIF.


END-OF-SELECTION.
