*&---------------------------------------------------------------------*
*& Report  ZZDD_PROGRAM_OBJECTS
*&
*&---------------------------------------------------------------------*
*& Purpose of this program is similar to environment analysis
*& It finds all objects used by higher level objects specified
*& for selection...
*& This program handles better FUGR type objects!
*&---------------------------------------------------------------------*

REPORT   zzdd_program_objects_v2.

* NOTE:
* include bellow is copied form SDN
INCLUDE: zzdd_where_used_list.

TABLES: e071.

SELECTION-SCREEN BEGIN OF SCREEN 1001.
SELECTION-SCREEN END OF SCREEN 1001.


SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE TEXT-b10.
SELECT-OPTIONS: s_object FOR e071-obj_name.
SELECTION-SCREEN END OF BLOCK b10.

SELECTION-SCREEN BEGIN OF BLOCK b20 WITH FRAME TITLE TEXT-b20.
PARAMETERS: p_used TYPE char01.
PARAMETERS: p_zonly AS CHECKBOX,
            p_summ  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b20.

TYPES: BEGIN OF ty_s_obj,
         pgmid     TYPE pgmid,
         object   TYPE trobjtype,
         obj_name TYPE sobj_name,
         devclass TYPE devclass,
       END OF ty_s_obj,
       ty_t_obj TYPE STANDARD TABLE OF ty_s_obj WITH DEFAULT KEY.
TYPES: BEGIN OF ty_s_tadir,
         pgmid      TYPE pgmid,
         object    TYPE trobjtype,
         obj_name  TYPE sobj_name,
         srcsystem  TYPE srcsystem,
         author    TYPE responsibl,
         devclass  TYPE devclass,
         delflag    TYPE objdelflag,
       END OF ty_s_tadir,
       ty_t_tadir TYPE STANDARD TABLE OF ty_s_tadir WITH DEFAULT KEY.

TYPES: BEGIN OF ty_s_objs,
         dir TYPE ty_s_obj,
         obj TYPE ty_s_obj,
       END OF ty_s_objs,
       ty_t_objs TYPE STANDARD TABLE OF ty_s_objs WITH DEFAULT KEY.

*DATA: list TYPE REF TO zcl_list2.

INITIALIZATION.
  p_used = abap_true.
  p_summ = abap_true.

START-OF-SELECTION.


  DATA: lt_tadir_sel TYPE ty_t_tadir.
* get object list from TADIR...
  IF s_object[] IS INITIAL.
    MESSAGE 'Object selection must be specified' TYPE 'I'.
    EXIT.
  ELSE.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_tadir_sel
      FROM tadir
     WHERE obj_name IN s_object
       AND delflag = abap_false.
  ENDIF.

  DATA: lt_objs    TYPE ty_t_objs,
        lv_title   TYPE string,
        lv_num5(5) TYPE n.

  IF p_used IS INITIAL.
    lv_title = 'SUB Objects'.
    PERFORM get_subobjects USING lt_tadir_sel
                                 lt_objs.
  ELSE.
    lv_title = 'USED Objects'.
    PERFORM get_used_objects USING lt_tadir_sel
                                   p_zonly
                                   p_summ
                                   lt_objs.
  ENDIF.

  DESCRIBE TABLE lt_objs.
  lv_num5 = sy-tfill.
  CONCATENATE lv_title '- Rcds:' lv_num5  INTO
                lv_title SEPARATED BY space.
*  CREATE OBJECT list.
*  list->display( it_data  = lt_objs
*                 iv_title = lv_title ).
*
*  CALL SELECTION-SCREEN 1001.

  PERFORM display_result USING lt_objs.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  GET_SUBOBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_TADIR_SEL  text
*      -->P_LT_SUBOBJECTS  text
*----------------------------------------------------------------------*
FORM get_subobjects  USING    it_tadir_sel  TYPE ty_t_tadir
                              rt_subs TYPE ty_t_objs.

  DATA: ls_object     TYPE lcl_dev_cross_ref=>type_s_e071_objkey,
        lt_subobjects TYPE lcl_dev_cross_ref=>type_t_e071_key.

  DATA: ls_subs TYPE ty_s_objs.
  FIELD-SYMBOLS: <dir> TYPE ty_s_tadir,
                 <so>  TYPE lcl_dev_cross_ref=>type_s_e071_objkey.

  LOOP AT it_tadir_sel ASSIGNING <dir>.
    MOVE-CORRESPONDING <dir> TO ls_object.
    ls_subs-dir-pgmid	= <dir>-pgmid.
    ls_subs-dir-object = <dir>-object.
    ls_subs-dir-obj_name = <dir>-obj_name.
    ls_subs-dir-devclass = <dir>-devclass.
    TRY .
        lcl_dev_cross_ref=>get_subobj(
            EXPORTING is_object    = ls_object
            IMPORTING et_subobject = lt_subobjects ).
        LOOP AT lt_subobjects ASSIGNING <so>.
          ls_subs-obj-pgmid	= <so>-pgmid.
          ls_subs-obj-object = <so>-object.
          ls_subs-obj-obj_name = <so>-obj_name.
          APPEND ls_subs TO rt_subs.
        ENDLOOP.
      CATCH lcx_dev_cross_ref_fm_call.
* empty subs
        APPEND ls_subs TO rt_subs.
    ENDTRY.
    CLEAR: lt_subobjects, ls_object, ls_subs.
  ENDLOOP.

ENDFORM.                    " GET_SUBOBJECTS
*&---------------------------------------------------------------------*
*&      Form  GET_USED_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_TADIR_SEL  text
*      -->P_LT_USED  text
*----------------------------------------------------------------------*
FORM get_used_objects  USING  it_tadir_sel  TYPE ty_t_tadir
                              iv_zonly
                              iv_summary
                              rt_used TYPE ty_t_objs.

  FIELD-SYMBOLS: <dir> TYPE ty_s_tadir.

  LOOP AT it_tadir_sel ASSIGNING <dir>.
    PERFORM get_req_obj USING  <dir>
                               rt_used.
  ENDLOOP.
  IF iv_zonly IS NOT INITIAL.
    DELETE rt_used WHERE obj-obj_name(1) <> 'Z'.
  ENDIF.

  IF iv_summary IS NOT INITIAL.
    SORT rt_used.
    DELETE ADJACENT DUPLICATES FROM rt_used.
  ENDIF.

ENDFORM.                    " GET_USED_OBJECTS
*&---------------------------------------------------------------------*
*&      Form  GET_PACKAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_USED_OBJ  text
*----------------------------------------------------------------------*
FORM get_package  CHANGING cs_obj TYPE ty_s_obj.

  SELECT SINGLE devclass
    INTO cs_obj-devclass
    FROM tadir
   WHERE pgmid    = cs_obj-pgmid
     AND object   = cs_obj-object
     AND obj_name = cs_obj-obj_name.
  IF sy-subrc <> 0.
    cs_obj-devclass = '???'.
  ENDIF.

ENDFORM.                    " GET_PACKAGE
*&---------------------------------------------------------------------*
*&      Form  GET_FUGR_OBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OBJECT  text
*      -->P_LT_EO71_KEY  text
*----------------------------------------------------------------------*
FORM get_fugr_obj USING iv_fugr_name
                        it_e071_key TYPE lcl_dev_cross_ref=>type_t_e071_rel
                        is_dir TYPE ty_s_tadir
                        et_used TYPE ty_t_objs.

  DATA: lt_e071_key TYPE lcl_dev_cross_ref=>type_t_e071_rel.


  FIELD-SYMBOLS: <key> TYPE lcl_dev_cross_ref=>type_s_e071_rel.
  DATA: lv_name    TYPE sobj_name,
        lv_len     TYPE i,
        ls_tadir   TYPE ty_s_tadir,
        lt_sub_key TYPE lcl_dev_cross_ref=>type_t_e071_rel,
        lt_used    TYPE ty_t_objs.

* entry for includes
  ls_tadir-pgmid = 'R3TR'.
  ls_tadir-object = 'PROG'.
  ls_tadir-devclass = is_dir-devclass.
* name of includes... Lxxx*
  CONCATENATE 'L' iv_fugr_name INTO lv_name.
  lv_len = strlen( lv_name ).

  LOOP AT it_e071_key ASSIGNING <key>.
    IF <key>-subobject-obj_name(lv_len) = lv_name. "fugr includes !
      ls_tadir-obj_name = <key>-subobject-obj_name.
      PERFORM get_req_obj USING  ls_tadir
                                 lt_used.
      APPEND LINES OF lt_used TO et_used.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_FUGR_OBJ
*&---------------------------------------------------------------------*
*&      Form  GET_REQ_OBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<DIR>  text
*      -->P_LT_USED  text
*----------------------------------------------------------------------*
FORM get_req_obj  USING    is_dir TYPE ty_s_tadir
                           rt_used TYPE ty_t_objs.

  DATA: ls_object   TYPE lcl_dev_cross_ref=>type_s_e071_objkey.
  DATA: lt_e071_key TYPE lcl_dev_cross_ref=>type_t_e071_rel.

  DATA: ls_used TYPE ty_s_objs.
  FIELD-SYMBOLS: <dir> TYPE ty_s_tadir,
                 <so>  TYPE lcl_dev_cross_ref=>type_s_e071_rel.

  MOVE-CORRESPONDING is_dir TO ls_object.
  ls_used-dir-pgmid	   = is_dir-pgmid.
  ls_used-dir-object   = is_dir-object.
  ls_used-dir-obj_name = is_dir-obj_name.
  ls_used-dir-devclass = is_dir-devclass.

  TRY .
      lcl_dev_cross_ref=>get_reqobj(
                  EXPORTING is_e071_key = ls_object
                  IMPORTING et_e071_key = lt_e071_key ).

      LOOP AT lt_e071_key ASSIGNING <so>.
        ls_used-obj-pgmid    = <so>-subobject-pgmid.
        ls_used-obj-object   = <so>-subobject-object.
        ls_used-obj-obj_name = <so>-subobject-obj_name.
        CLEAR ls_used-obj-devclass.
        IF ls_used-obj-pgmid = 'R3TR'.
          PERFORM get_package CHANGING ls_used-obj.
        ENDIF.
        APPEND ls_used TO rt_used.
      ENDLOOP.
      IF is_dir-object = 'FUGR'.
        PERFORM get_fugr_obj USING is_dir-obj_name
                                   lt_e071_key
                                   is_dir
                                   rt_used.
      ENDIF.
    CATCH cx_enh_root.
* empty subs
      APPEND ls_used TO rt_used.
  ENDTRY.

ENDFORM.                    " GET_REQ_OBJ
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJS  text
*----------------------------------------------------------------------*
FORM display_result  USING    it_objs TYPE ty_t_objs.

  DATA(lo_dsplist) = NEW /river/cl_salv_list(  ).

  lo_dsplist->display( it_objs ).


ENDFORM.
