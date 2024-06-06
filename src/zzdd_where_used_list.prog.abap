*&---------------------------------------------------------------------*
*&  Include           ZZDD_WHERE_USED_LIST
*&---------------------------------------------------------------------*
********************
* INCLUDE Z_DEV_WHERE_USED_LIST
*
* Current version : 1.01
* Version date    : 2010-11-22
*
********************
* REVISIONS
*   2010-11-22  1.01  :  correction about fictious <name>=======P include
*   2010-04-30  1.00  :  Official release
*   2009-12-13  0.01  :  Creation
*
********************

*----------------------------------------------------------------------*
*       CLASS lcl_tran DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_tran DEFINITION.
  PUBLIC SECTION.

    TYPES : BEGIN OF type_s_cobj,
              field TYPE tstca-field,
              value TYPE tstca-value,
              olen  TYPE dfies-outputlen,
            END OF type_s_cobj.
    TYPES type_t_cobj TYPE TABLE OF type_s_cobj.

* report transaction
    TYPES : BEGIN OF type_s_report,
              program_name      TYPE tstc-pgmna,
              screen_number     TYPE tstc-dypno,
              program_variant   TYPE rsstcd-repo_vari,
              auth_object       TYPE tstca-objct,
              t_cobj            TYPE TABLE OF type_s_cobj WITH DEFAULT KEY,
            END OF type_s_report.
* dialog transaction
    TYPES : BEGIN OF type_s_dialog,
              program_name      TYPE tstc-pgmna,
              screen_number     TYPE tstc-dypno,
              allow_std_transac_variant TYPE flag,
              auth_object       TYPE tstca-objct,
              t_cobj            TYPE TABLE OF type_s_cobj WITH DEFAULT KEY,
            END OF type_s_dialog.
* parameter transaction
    TYPES : BEGIN OF type_s_parameter,
              called_tcode      TYPE tstc-tcode,
              skip_init_screen  TYPE flag,
              inherit_gui_attr  TYPE flag,
              program_name      TYPE tstc-pgmna,
              screen_number     TYPE tstc-dypno,
              t_param           TYPE s_param,
            END OF type_s_parameter.
* variant transaction
    TYPES : BEGIN OF type_s_variant,
              called_tcode      TYPE tstc-tcode,
              transac_variant   TYPE rsstcd-variant,
              cross_client      TYPE flag,
              inherit_gui_attr  TYPE flag,
            END OF type_s_variant.
* object transaction
    TYPES : BEGIN OF type_s_object,
              transaction_model TYPE flag,
              local_class       TYPE flag,
              global_class_name TYPE seoclsname,
              local_class_name  TYPE seoclsname,
              method_name       TYPE seocpdname,
              program_name      TYPE tstc-pgmna,
              update_mode       TYPE char01, "only for transaction model
              auth_object       TYPE tstca-objct,
              t_cobj            TYPE TABLE OF type_s_cobj WITH DEFAULT KEY,
            END OF type_s_object.

    DATA :    tcode        TYPE tstc-tcode,
              type         TYPE char01,
              s_report     TYPE type_s_report,
              s_dialog     TYPE type_s_dialog,
              s_object     TYPE type_s_object,
              s_parameter  TYPE type_s_parameter,
              s_variant    TYPE type_s_variant,
              locked_via_sm01 TYPE flag,
              professional TYPE flag,
              easy_web     TYPE flag,
              ew_service   TYPE tstcc-s_service, "easy web
              ew_pervasive TYPE tstcc-s_pervas, "easy web
              gui_html     TYPE tstcc-s_webgui,
              gui_win32    TYPE tstcc-s_win32,
              gui_java     TYPE tstcc-s_platin.

    METHODS constructor IMPORTING i_tcode TYPE tcode.

ENDCLASS.                    "lcl_tran DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_tran IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tran IMPLEMENTATION.

  METHOD constructor.

    FIELD-SYMBOLS <ls_tstc> TYPE tstc.
    FIELD-SYMBOLS <ls_tstcc> TYPE tstcc.
    FIELD-SYMBOLS <lt_cobj> TYPE type_t_cobj.
    FIELD-SYMBOLS <ls_rsstcd> TYPE rsstcd.
    FIELD-SYMBOLS <lt_param> TYPE s_param.
    FIELD-SYMBOLS <l_easy_web> TYPE flag.
    FIELD-SYMBOLS <l_professional> TYPE flag.
    FIELD-SYMBOLS <l_transaction_variant_flag> TYPE flag.
    FIELD-SYMBOLS <l_transaction_type> TYPE tstc-cinfo.
    FIELD-SYMBOLS <l_authorization_object_flag> TYPE syhex01.
    FIELD-SYMBOLS <l_locked_via_sm01> TYPE syhex01.
    FIELD-SYMBOLS <l_gui_inherit> TYPE flag.
    DATA l_auth_object TYPE tstca-objct.

    PERFORM select_tstc_tables_new IN PROGRAM saplseuk
          USING i_tcode sy-langu sy-langu.

    ASSIGN ('(SAPLSEUK)TSTC') TO <ls_tstc>.
    ASSIGN ('(SAPLSEUK)TSTCC') TO <ls_tstcc>.
    ASSIGN ('(SAPLSEUK)COBJ[]') TO <lt_cobj>.
    ASSIGN ('(SAPLSEUK)RSSTCD') TO <ls_rsstcd>.
    ASSIGN ('(SAPLSEUK)PARAM[]') TO <lt_param>.
    ASSIGN ('(SAPLSEUK)G_IAC_EWT') TO <l_easy_web>.
    ASSIGN ('(SAPLSEUK)G_PROFI_TRAN') TO <l_professional>.

    ASSIGN ('(SAPLSEUK)PARAM_VARI') TO <l_transaction_variant_flag>.
    ASSIGN ('(SAPLSEUK)TC_TYP') TO <l_transaction_type>.
    ASSIGN ('(SAPLSEUK)TC_CHK') TO <l_authorization_object_flag>.
    ASSIGN ('(SAPLSEUK)TC_ENQ') TO <l_locked_via_sm01>.
    ASSIGN ('(SAPLSEUK)G_GUI_INHE') TO <l_gui_inherit>.

    tcode = i_tcode.

    IF NOT <l_authorization_object_flag> IS INITIAL.
      SELECT SINGLE objct FROM tstca
            INTO l_auth_object
            WHERE tcode = i_tcode.
    ENDIF.

    CASE <l_transaction_type>.
      WHEN '80'.
        type = 'R'.
        s_report-program_name = <ls_tstc>-pgmna.
        s_report-screen_number = <ls_tstc>-dypno.
        s_report-program_variant = <ls_rsstcd>-repo_vari.
        s_report-auth_object = l_auth_object.
        s_report-t_cobj = <lt_cobj>.
      WHEN '00'.
        type = 'D'.
        s_dialog-program_name  = <ls_tstc>-pgmna.
        s_dialog-screen_number = <ls_tstc>-dypno.
        s_dialog-allow_std_transac_variant = <ls_rsstcd>-trans_var.
        s_dialog-auth_object   = l_auth_object.
        s_dialog-t_cobj        = <lt_cobj>.
      WHEN '01'.
        type = 'M'. "menu area (obsolete transaction type)
      WHEN '08'.
        type = 'O'.
        IF <ls_rsstcd>-call_tcode = 'OS_APPLICATION'.
          s_object-transaction_model = 'X'.
          s_object-global_class_name = <ls_rsstcd>-classname.
* Update mode is stored in TSTCP-PARM like %UPDATE_MODE=?%
          IF <ls_rsstcd>-s_upddir = 'X'.
            s_object-update_mode = 'S'.
          ELSEIF <ls_rsstcd>-s_updtask = 'X'.
            s_object-update_mode = 'A'.
          ELSEIF <ls_rsstcd>-s_updlok = 'X'.
            s_object-update_mode = 'L'.
          ENDIF.
        ELSE.
          IF NOT <ls_tstc>-pgmna IS INITIAL.
            s_object-local_class       = 'X'.
            s_object-program_name      = <ls_tstc>-pgmna.
            s_object-local_class_name  = <ls_rsstcd>-classname.
          ELSE.
            s_object-global_class_name = <ls_rsstcd>-classname.
          ENDIF.
        ENDIF.
        s_object-method_name   = <ls_rsstcd>-method.
        s_object-auth_object   = l_auth_object.
        s_object-t_cobj        = <lt_cobj>.
      WHEN '02'.
        IF <l_transaction_variant_flag> = 'X'.
          type = 'V'. "variant transaction
          s_variant-called_tcode      = <ls_rsstcd>-call_tcode.
          s_variant-transac_variant   = <ls_rsstcd>-variant.
          s_variant-cross_client      = <ls_rsstcd>-s_ind_vari.
          s_variant-inherit_gui_attr  = <l_gui_inherit>.
        ELSE.
          type = 'P'. "parameter transaction
          s_parameter-called_tcode      = <ls_rsstcd>-call_tcode.
          s_parameter-skip_init_screen  = <ls_rsstcd>-st_skip_1.
          s_parameter-inherit_gui_attr  = <l_gui_inherit>.
          s_parameter-program_name      = <ls_tstc>-pgmna.
          s_parameter-screen_number     = <ls_tstc>-dypno.
          s_parameter-t_param           = <lt_param>.
        ENDIF.
    ENDCASE.

    IF NOT <l_locked_via_sm01> IS INITIAL.
      locked_via_sm01 = 'X'.
    ENDIF.

    professional = <l_professional>.
    easy_web = <l_easy_web>.
    ew_service   = <ls_tstcc>-s_service.
    ew_pervasive = <ls_tstcc>-s_pervas.
    gui_html     = <ls_tstcc>-s_webgui.
    gui_win32    = <ls_tstcc>-s_win32.
    gui_java     = <ls_tstcc>-s_platin.

  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_tran IMPLEMENTATION


INCLUDE ttypleng.
*----------------------------------------------------------------------*
*       CLASS LCL_convert_wbobj_key_2_e071 DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_convert_wbobj_key_2_e071 DEFINITION.
  PUBLIC SECTION.
    TYPES : BEGIN OF type_s_docu_key,
              docu_id      TYPE dokhl-id,
              docu_object  TYPE dokhl-object,
            END OF type_s_docu_key.
    TYPES : BEGIN OF type_s_tobj_key,
              objectname   TYPE objh-objectname,
              objecttype   TYPE objh-objecttype,
            END OF type_s_tobj_key.
    TYPES : BEGIN OF type_s_sott_key,
              paket        TYPE sotr_pack,
              concept      TYPE sotr_conc,
            END OF type_s_sott_key.
    TYPES type_spad(5) TYPE c.
    TYPES : BEGIN OF type_s_spcs_key,
              spad_type TYPE type_spad,
              codepage  TYPE tcp00-cpcodepage,
            END OF type_s_spcs_key.
    TYPES : BEGIN OF type_s_spsv_key,
              spad_type  TYPE type_spad,
              server     TYPE tspsv-server,
            END OF type_s_spsv_key.
    TYPES : BEGIN OF type_s_spdv_key,
              spad_type  TYPE type_spad,
              device     TYPE tsp03-padest,
            END OF type_s_spdv_key.
    TYPES : BEGIN OF type_s_splo_key,
              spad_type     TYPE type_spad,
              paper_format  TYPE tsp1d-papart,
            END OF type_s_splo_key.
    TYPES : BEGIN OF type_s_prin_key,
              spad_type     TYPE type_spad,
              printer_type  TYPE tsp0a-patype,
            END OF type_s_prin_key.
    TYPES : BEGIN OF type_s_slom_key,
              spad_type              TYPE type_spad,
              logical_output_system  TYPE tsploms-name,
            END OF type_s_slom_key.
    TYPES : BEGIN OF type_s_soms_key,
              spad_type          TYPE type_spad,
              read_output_system TYPE tsproms-name,
            END OF type_s_soms_key.
    TYPES : BEGIN OF type_s_scp_key,
              bcset_id TYPE scpr_id,
              category TYPE scpr_ctgry,
            END OF type_s_scp_key.
    TYPES : BEGIN OF type_s_dynp_key,
              program_name   TYPE d020s-prog,
              screen_number  TYPE d020s-dnum,
            END OF type_s_dynp_key.
    TYPES : BEGIN OF type_s_vari_key,
              variant_name TYPE vari-variant,
              program_name TYPE vari-report,
            END OF type_s_vari_key.
    TYPES : BEGIN OF type_s_mess_key,
              msg_class_name  TYPE t100-arbgb,
              msg_number      TYPE t100-msgnr,
            END OF type_s_mess_key.
    TYPES : BEGIN OF type_s_meth_key,
              class_name   TYPE seoclsname,
              method_name  TYPE seocpdname,
            END OF type_s_meth_key.
    TYPES : BEGIN OF type_s_wdyc_key,
              webdynpro_name    TYPE wdy_component_name,
              controller_name   TYPE wdy_controller_name,
            END OF type_s_wdyc_key.
    TYPES : BEGIN OF type_s_wdyv_key,
              webdynpro_name    TYPE wdy_component_name,
              view_name         TYPE wdy_view_name,
            END OF type_s_wdyv_key.
    TYPES : BEGIN OF type_s_wapp_key,
              appl_name     TYPE o2applname,
              page_name     TYPE o2page,
            END OF type_s_wapp_key.
    TYPES : BEGIN OF type_s_wbobj_key,
*          obj_name   TYPE e071-obj_name,
          object     TYPE e071-object, "DYNP, PROG, FUGR, FUNC, etc.
          s_docu     TYPE type_s_docu_key,
          include    TYPE progname,
          trkorr     TYPE trkorr,
          s_tobj     TYPE type_s_tobj_key,
          s_sott     TYPE type_s_sott_key,
          s_spcs     TYPE type_s_spcs_key,
          s_spsv     TYPE type_s_spsv_key,
          s_spdv     TYPE type_s_spdv_key,
          s_splo     TYPE type_s_splo_key,
          s_prin     TYPE type_s_prin_key,
          s_slom     TYPE type_s_slom_key,
          s_soms     TYPE type_s_soms_key,
          s_scp      TYPE type_s_scp_key,
          file       TYPE cts_guid32,
          s_dynp     TYPE type_s_dynp_key,
          s_vari     TYPE type_s_vari_key,
          s_mess     TYPE type_s_mess_key,
          s_meth     TYPE type_s_meth_key,
          s_wdyc     TYPE type_s_wdyc_key,
          s_wdyv     TYPE type_s_wdyv_key,
          s_wapp     TYPE type_s_wapp_key,
        END OF type_s_wbobj_key.
    CLASS-METHODS execute
          IMPORTING
            is_wbobj_key    TYPE  type_s_wbobj_key
          CHANGING
            es_e071_key     TYPE  CTS_OBJECT. "if error, try with CTS_OBJECT_KEY
ENDCLASS.                    "LCL_convert_wbobj_key_2_e071 DEFINITION



*----------------------------------------------------------------------*
*       CLASS LCL_convert_wbobj_key_2_e071 DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_convert_wbobj_key_2_e071 IMPLEMENTATION.
  METHOD execute.
    DATA ls_ko100 TYPE ko100.

* determine if type (PROG, CLAS, DYNP, etc.) corresponds to R3TR or LIMU
    CALL FUNCTION 'TR_GET_PGMID_FOR_OBJECT'
      EXPORTING
        iv_object      = is_wbobj_key-object
      IMPORTING
        es_type        = ls_ko100
      EXCEPTIONS
        illegal_object = 1
        OTHERS         = 2.

    CLEAR es_e071_key.
    es_e071_key-pgmid  = ls_ko100-pgmid. "usually R3TR or LIMU
    es_e071_key-object = is_wbobj_key-object.

    CASE is_wbobj_key-object.

      WHEN 'DOCU'.
        es_e071_key-obj_name(2) = is_wbobj_key-s_docu-docu_id.
        es_e071_key-obj_name+2 = is_wbobj_key-s_docu-docu_object.


      WHEN 'SOTT' OR 'SOTU'.
        es_e071_key-obj_name(30) = is_wbobj_key-s_sott-paket.
        es_e071_key-obj_name+30 = is_wbobj_key-s_sott-concept.


      WHEN 'TOBJ'.
        CONCATENATE is_wbobj_key-s_tobj-objectname
                    is_wbobj_key-s_tobj-objecttype
            INTO es_e071_key-obj_name.


      WHEN 'MERG' OR 'RELE' OR 'COMM'.
        es_e071_key-obj_name = is_wbobj_key-trkorr.


      WHEN 'SPCS'.
        es_e071_key-obj_name = is_wbobj_key-s_spcs-codepage.
      WHEN 'SPSV'.
        es_e071_key-obj_name = is_wbobj_key-s_spsv-server.
      WHEN 'SPDV'.
        SELECT SINGLE name FROM tsp03d INTO es_e071_key-obj_name
              WHERE padest = is_wbobj_key-s_spdv-device.
      WHEN 'SPLO'.
        es_e071_key-obj_name = is_wbobj_key-s_splo-paper_format.
      WHEN 'PRIN'.
        es_e071_key-obj_name = is_wbobj_key-s_prin-printer_type.
      WHEN 'SLOM'.
        es_e071_key-obj_name = is_wbobj_key-s_slom-logical_output_system.
      WHEN 'SOMS'.
        es_e071_key-obj_name = is_wbobj_key-s_soms-read_output_system.


      WHEN 'SCP1'.
        es_e071_key-obj_name = is_wbobj_key-s_scp-bcset_id.
      WHEN 'SCP2'.
        es_e071_key-obj_name = is_wbobj_key-s_scp-bcset_id.


      WHEN 'FILE'.
        es_e071_key-obj_name = is_wbobj_key-file.

      WHEN 'REPO'.
        es_e071_key-obj_name = is_wbobj_key-include.

      WHEN 'DYNP'.
        es_e071_key-pgmid    = 'LIMU'.
        es_e071_key-obj_name+gc_prog(gc_dynp) = is_wbobj_key-s_dynp-screen_number.
        es_e071_key-obj_name(gc_prog)         = is_wbobj_key-s_dynp-program_name.

      WHEN 'VARI' OR 'VARX'.
        es_e071_key-obj_name+gc_prog(gc_vari) = is_wbobj_key-s_vari-variant_name.
        es_e071_key-obj_name(gc_prog)         = is_wbobj_key-s_vari-program_name.

      WHEN 'MESS'.
        es_e071_key-pgmid    = 'LIMU'.
        CONCATENATE is_wbobj_key-s_mess-msg_class_name
                    is_wbobj_key-s_mess-msg_number
              INTO es_e071_key-obj_name.

      WHEN 'METH'.
        es_e071_key-pgmid    = 'LIMU'.
        es_e071_key-obj_name+gc_clas(gc_meth) = is_wbobj_key-s_meth-method_name.
        es_e071_key-obj_name(gc_clas)         = is_wbobj_key-s_meth-class_name.

* Web Dynpro controller
      WHEN 'WDYC'.
        es_e071_key-obj_name+gc_wdyn(gc_wdyc) = is_wbobj_key-s_wdyc-controller_name.
        es_e071_key-obj_name(gc_wdyn) = is_wbobj_key-s_wdyc-webdynpro_name.

* Web Dynpro view
      WHEN 'WDYV'.
        es_e071_key-obj_name+gc_wdyn(gc_wdyv) = is_wbobj_key-s_wdyv-view_name.
        es_e071_key-obj_name(gc_wdyn) = is_wbobj_key-s_wdyv-webdynpro_name.

* Page/Controller of a BSP Application
      WHEN 'WAPD' OR 'WAPP'.
        es_e071_key-obj_name+gc_wapa(gc_wapp) = is_wbobj_key-s_wapp-page_name.
        es_e071_key-obj_name(gc_wapa) = is_wbobj_key-s_wapp-appl_name.

      WHEN OTHERS.
* FUGR, CLAS, FUNC, PROG, etc.
        es_e071_key-obj_name = is_wbobj_key-object.

    ENDCASE.
  ENDMETHOD.                    "execute
ENDCLASS.                    "LCL_convert_wbobj_key_2_e071 DEFINITION


*----------------------------------------------------------------------*
CLASS lcl_sap_fm_ddif_typeinfo_get DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS execute
      IMPORTING
        i_tabname TYPE tabname
      CHANGING
        e_object TYPE ddtypekind.
ENDCLASS.                    "lcl_sap_fm_DDIF_TYPEINFO_GET DEFINITION

*----------------------------------------------------------------------*
CLASS lcl_sap_fm_ddif_typeinfo_get IMPLEMENTATION.
  METHOD execute.
    DATA l_typename TYPE typename.
    DATA l_typekind TYPE ddtypekind.

    CLEAR e_object.

    l_typename = i_tabname.
    CALL FUNCTION 'DDIF_TYPEINFO_GET'
      EXPORTING
        typename = l_typename
      IMPORTING
        typekind = l_typekind.
    e_object = l_typekind.
  ENDMETHOD.                    "execute
ENDCLASS.                    "lcl_sap_fm_DDIF_TYPEINFO_GET IMPLEMENTATION


*----------------------------------------------------------------------*
CLASS lcl_sap_fm_tr_check_type DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS execute
        IMPORTING
          wi_e071  TYPE e071
        EXPORTING
          we_lock_key TYPE tlock_int
          we_tadir TYPE tadir.
ENDCLASS.                    "lcl_sap_fm DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_sap_fm_tr_check_type IMPLEMENTATION.
  METHOD execute.
    DATA ls_e071 TYPE e071.
* TR_CHECK_TYPE returns empty when object = VARX !
* With VARI, it returns program name
    ls_e071 = wi_e071.
    IF ls_e071-object = 'VARX'.
      ls_e071-object = 'VARI'.
    ENDIF.
    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071     = ls_e071
      IMPORTING
        we_tadir    = we_tadir
        we_lock_key = we_lock_key.
  ENDMETHOD.                    "TR_CHECK_TYPE
ENDCLASS.                    "lcl_sap_fm IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcx_dev_cross_ref_fm_call DEFINITION
*----------------------------------------------------------------------*
CLASS lcx_dev_cross_ref_fm_call DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS constructor.
    CONSTANTS fffff TYPE sotr_conc VALUE '86BE4E4B5D93F764E1000000AC12017F'.
    CLASS-DATA fbname2 TYPE rs38l_fnam.
    CLASS-DATA excname2 TYPE rs38l_exce.
    DATA fbname TYPE rs38l_fnam.
    DATA excname TYPE rs38l_exce.
ENDCLASS.                    "lcx_dev_cross_ref_fm_call DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcx_dev_cross_ref_fm_call IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcx_dev_cross_ref_fm_call IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    fbname = fbname2.
    excname = excname2.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcx_dev_cross_ref_fm_call IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_dev_cross_ref DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_dev_cross_ref DEFINITION.
  PUBLIC SECTION.
    TYPES : BEGIN OF type_s_e071_objkey,
              pgmid     TYPE e071-pgmid,
              object    TYPE e071-object,
              obj_name  TYPE e071-obj_name,
            END OF type_s_e071_objkey.
    TYPES type_t_e071_key TYPE STANDARD TABLE OF type_s_e071_objkey.
    TYPES soft_or_hard TYPE c LENGTH 1.
    TYPES : BEGIN OF type_s_e071_rel,
              soft_or_hard TYPE soft_or_hard,
              subobject TYPE type_s_e071_objkey,
            END OF type_s_e071_rel.
    TYPES type_t_e071_rel TYPE STANDARD TABLE OF type_s_e071_rel.

    CLASS-METHODS get_reqobj
          IMPORTING
            is_e071_key   TYPE type_s_e071_objkey
          EXPORTING
            et_e071_key   TYPE type_t_e071_rel
          RAISING
            cx_enh_root.
    CLASS-METHODS get_subobj
          IMPORTING
            is_object     TYPE type_s_e071_objkey
          EXPORTING
            et_subobject  TYPE type_t_e071_key
          RAISING
            lcx_dev_cross_ref_fm_call.

  PRIVATE SECTION.
    CLASS-DATA gt_e071_key TYPE type_t_e071_key.

    CLASS-METHODS get_include_required_objects
          IMPORTING
            i_include     TYPE progname.
    CLASS-METHODS get_devc_required_objects
          IMPORTING
            i_package_name TYPE devclass.
    CLASS-METHODS tablstruc_field
        IMPORTING
          is_dd03p    TYPE dd03p
          is_e071_key TYPE type_s_e071_objkey.
    CLASS-METHODS collect
          IMPORTING
            soft_or_hard  TYPE soft_or_hard
            y             TYPE type_s_e071_objkey.  "subobject
ENDCLASS.                    "lcl_dev_cross_ref DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_dev_cross_ref IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_dev_cross_ref IMPLEMENTATION.
  DEFINE mac_collect.
    assert &1 = &1. assert &3 = &3. call method collect
      exporting
        soft_or_hard = &2
        y            = &4.
  END-OF-DEFINITION.
  METHOD get_subobj.
    DATA lt_t100     TYPE TABLE OF t100.
    DATA ls_t100     TYPE t100.
    DATA lt_varid    TYPE TABLE OF varid.
    DATA ls_varid    TYPE varid.
    DATA ls_e071_key TYPE type_s_e071_objkey.
    DATA ls_e071     TYPE e071.
    DATA lt_vrso_source TYPE TABLE OF vrso.
    DATA ls_vrso        TYPE vrso.

    REFRESH et_subobject.

    IF is_object-pgmid = 'R3TR'.
      CASE is_object-object.
        WHEN 'MSAG'.
          SELECT * FROM t100 INTO TABLE lt_t100 WHERE arbgb = is_object-obj_name.
          LOOP AT lt_t100 INTO ls_t100.
            ls_e071_key-pgmid    = 'LIMU'.
            ls_e071_key-object   = 'MESS'.
            CONCATENATE is_object-obj_name ls_t100-msgnr INTO ls_e071_key-obj_name.
            APPEND ls_e071_key TO et_subobject.
          ENDLOOP.
        WHEN OTHERS.

* Call TRINT_RESOLVE_OBJ on each system to check presence of all subobjects
*   Caution: this subobjets n'existent pas forcément.
* If subobject belongs to a frame object, check that this last one exists in the
*   same transport request or one transported before.
*   For example, if it contains FUGR object, all its LIMU FUNC are
*   also transported.
          ls_e071-object = is_object-object.
          ls_e071-obj_name = is_object-obj_name.
          REFRESH lt_vrso_source.
* FM deleted as of 740 !
*          CALL FUNCTION 'TRINT_RESOLVE_OBJ'
*            EXPORTING
*              is_e071             = ls_e071
*            TABLES
*              et_vrso             = lt_vrso_source
*            EXCEPTIONS
*              not_versionable     = 1
*              communication_error = 2
*              OTHERS              = 3.
*DATA E071_OBJ TYPE E071.
*DATA OBJ_TAB  TYPE STANDARD TABLE OF VRSO.
          CALL FUNCTION 'SVRS_RESOLVE_E071_OBJ'
            EXPORTING
              e071_obj              = ls_e071
            TABLES
              obj_tab               = lt_vrso_source
           EXCEPTIONS
             NOT_VERSIONABLE       = 1
             OTHERS                = 2 .
          IF sy-subrc <> 0.
            DEFINE exception1.
              lcx_dev_cross_ref_fm_call=>excname2 = &1.
              raise exception type lcx_dev_cross_ref_fm_call.
            END-OF-DEFINITION.
            lcx_dev_cross_ref_fm_call=>fbname2 = 'TRINT_RESOLVE_OBJ'.
            CASE sy-subrc.
              WHEN 1. exception1 'NOT_VERSIONABLE'.
              WHEN 2. exception1 'COMMUNICATION_ERROR'.
              WHEN 3. exception1 'OTHERS'.
            ENDCASE.

          ENDIF.
          LOOP AT lt_vrso_source INTO ls_vrso.
            ls_e071_key-pgmid    = 'LIMU'.
            ls_e071_key-object   = ls_vrso-objtype.
            ls_e071_key-obj_name = ls_vrso-objname.
            APPEND ls_e071_key TO et_subobject.
          ENDLOOP.
      ENDCASE.
    ENDIF.

    CASE is_object-object.
      WHEN 'PROG' OR 'REPS' OR 'REPO'.
* propose automatically system variants
        SELECT * FROM varid CLIENT SPECIFIED
              INTO TABLE lt_varid
              WHERE mandt     = '000'  "system variants are only in client 000
                AND report    = is_object-obj_name
                AND transport = space. "system variant
        LOOP AT lt_varid INTO ls_varid.
          ls_e071_key-pgmid    = 'LIMU'.
          ls_e071_key-object   = 'VARX'.
          CONCATENATE ls_varid-report ls_varid-variant INTO ls_e071_key-obj_name RESPECTING BLANKS.
          APPEND ls_e071_key TO et_subobject.
        ENDLOOP.
    ENDCASE.

  ENDMETHOD.                    "get_subobj

  METHOD collect.
    DATA ls_e071_key TYPE type_s_e071_rel.

    ls_e071_key-soft_or_hard = soft_or_hard.
    ls_e071_key-subobject    = y.
    APPEND ls_e071_key TO gt_e071_key.

  ENDMETHOD.                    "collect

  METHOD get_devc_required_objects.
    DATA lo_package TYPE REF TO if_package.
    DATA ls_e071_key TYPE type_s_e071_objkey.
    DATA is_e071_key TYPE type_s_e071_objkey.

    CALL METHOD cl_package=>load_package
      EXPORTING
        i_package_name             = i_package_name
      IMPORTING
        e_package                  = lo_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        object_locked_and_modified = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
    ENDIF.
    IF lo_package IS BOUND.
* SUB-PACKAGES
      DATA lto_package_sub TYPE scompaklis.
      CALL METHOD lo_package->get_sub_packages
        IMPORTING
          e_sub_packages   = lto_package_sub
        EXCEPTIONS
          object_invalid   = 1
          leaf_package     = 2
          unexpected_error = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
      ENDIF.
      DATA lo_package_sub TYPE REF TO if_package.
      LOOP AT lto_package_sub INTO lo_package_sub.
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'DEVC'.
        ls_e071_key-obj_name = lo_package_sub->package_name.
        mac_collect is_e071_key 'S' '' ls_e071_key.
      ENDLOOP.
* SUPER PACKAGE
      DATA lo_package_super TYPE REF TO if_package.
      CALL METHOD lo_package->get_super_package
        IMPORTING
          e_super_package = lo_package_super
        EXCEPTIONS
          root_package    = 1
          OTHERS          = 2.
      IF sy-subrc = 0.

        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'DEVC'.
        ls_e071_key-obj_name = lo_package_super->package_name.
        mac_collect is_e071_key 'H' '' ls_e071_key.

        DATA lo_interface TYPE REF TO if_package_interface.
        DATA lto_interface TYPE tpak_package_interface_list.
        CALL METHOD lo_package->get_interfaces
          IMPORTING
            e_package_interfaces = lto_interface.
        LOOP AT lto_interface INTO lo_interface.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'PINF'.
          ls_e071_key-obj_name = lo_interface->interface_name.
          mac_collect is_e071_key 'S' '' ls_e071_key.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "get_devc_required_objects

  METHOD get_include_required_objects.
    DATA ls_e071_key TYPE type_s_e071_objkey.
    DATA lt_incl TYPE TABLE OF rseuinc.
    FIELD-SYMBOLS <ls_incl> TYPE rseuinc.
    DATA lt_cross TYPE TABLE OF cross.
    FIELD-SYMBOLS <ls_cross> TYPE cross.
    DATA l_progname TYPE syrepid.
    DATA l_subc TYPE trdir-subc.
    DATA lte_main TYPE TABLE OF d010inc-master.
    FIELD-SYMBOLS <l_main> TYPE d010inc-master.
    DATA l_object TYPE e071-object.
    DATA ls_wbobj_key TYPE lcl_convert_wbobj_key_2_e071=>type_s_wbobj_key.
    DATA l_class_is_name TYPE  c.
    DATA l_class_name TYPE  seoclsname.
    DATA l_class_is_method_name TYPE  c.
    DATA l_class_method_name TYPE  seocpdname.
    DATA is_e071_key TYPE type_s_e071_objkey.

    l_progname = i_include.


* Get list of INCLUDE statements.
    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = l_progname
      TABLES
        includetab   = lt_incl
      EXCEPTIONS
        not_existent = 0
        no_program   = 0
        OTHERS       = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
* 2010-11-22
* D010INC table may sometimes contain an erroneous entry:
* for classname===...===CP, RS_GET_ALL_INCLUDES returns
* packagename===...===P that doesn't exist
    DELETE lt_incl WHERE master+30 = 'P'.

* For each include, get all objects it is using.
    LOOP AT lt_incl ASSIGNING <ls_incl>.

* Determine the type of the include
      CALL FUNCTION 'RS_PROGNAME_SPLIT'
        EXPORTING
          progname_with_namespace = <ls_incl>-master
        IMPORTING
          class_is_name           = l_class_is_name
          class_name              = l_class_name
          class_is_method_name    = l_class_is_method_name
          class_method_name       = l_class_method_name
        EXCEPTIONS
          delimiter_error         = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF 0 = 1.
      ELSEIF l_class_is_name = 'X'.
* The include refers to a class section or class frame program
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'CLAS'.
        ls_e071_key-obj_name = l_class_name.
        mac_collect is_e071_key 'H' '' ls_e071_key .
      ELSEIF l_class_is_method_name = 'X'.
* The include refers to a class method
        ls_wbobj_key-object = 'METH'.
        ls_wbobj_key-s_meth-class_name = l_class_name.
        ls_wbobj_key-s_meth-method_name = l_class_method_name.
        CALL METHOD lcl_convert_wbobj_key_2_e071=>execute
          EXPORTING
            is_wbobj_key = ls_wbobj_key
          CHANGING
            es_e071_key  = ls_e071_key.
        mac_collect is_e071_key 'H' '' ls_e071_key .
      ELSE.
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'PROG'.
        ls_e071_key-obj_name = <ls_incl>-master.
        mac_collect is_e071_key 'H' '' ls_e071_key .
      ENDIF.
    ENDLOOP.


* Get all DDIC types referenced in the include
    DATA lt_wbcrossgt TYPE TABLE OF wbcrossgt.
    DATA ls_wbcrossgt TYPE wbcrossgt.
    DATA l_ddictype TYPE wbcrossgt-name.
    DATA l_remain TYPE wbcrossgt-name.
    SELECT * FROM wbcrossgt INTO TABLE lt_wbcrossgt
          WHERE include = i_include
            AND otype = 'TY'.
    LOOP AT lt_wbcrossgt INTO ls_wbcrossgt.
      SPLIT ls_wbcrossgt-name AT '\' INTO l_ddictype l_remain.
* Get ddic object type
      IF l_remain IS INITIAL.
        DATA l_tabname TYPE tabname.
        l_tabname = l_ddictype.
        CALL METHOD lcl_sap_fm_ddif_typeinfo_get=>execute
          EXPORTING
            i_tabname = l_tabname
          CHANGING
            e_object  = l_object.
        IF l_object IS INITIAL.
* If it's not a DDIC type, then it's a class or interface
          DATA l_clstype TYPE seoclass-clstype.
          SELECT SINGLE clstype FROM seoclass INTO l_clstype
                WHERE clsname = l_ddictype.
          IF sy-subrc = 0.
            IF l_clstype = 0.
              l_object = 'CLAS'.
            ELSE.
              l_object = 'INTF'.
            ENDIF.
          ENDIF.
        ENDIF.
        IF l_object IS NOT INITIAL.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = l_object.
          ls_e071_key-obj_name = l_ddictype.
          mac_collect is_e071_key 'H' '' ls_e071_key.
        ENDIF.
      ELSE.
        l_remain = l_remain+3. "remove TY:
        DATA ls_dd03p TYPE dd03p.
        SELECT SINGLE * FROM dd03l INTO CORRESPONDING FIELDS OF ls_dd03p
              WHERE tabname = l_ddictype
                AND fieldname = l_remain
                AND as4local = 'A'
                AND as4vers = 0.
        IF sy-subrc = 0.
          CALL METHOD tablstruc_field
            EXPORTING
              is_dd03p    = ls_dd03p
              is_e071_key = is_e071_key.
        ENDIF.
      ENDIF.
* Note: we don't want to know if data/type comes from a type-pool
* because it's a little bit complex (DA and TY); instead, we use the
* type-pools declarations referenced in CROSS table (see below)
    ENDLOOP.

* Note: cross-name = "?" for objects dynamically called.
    SELECT * FROM cross
        INTO TABLE lt_cross
        WHERE include = i_include
          AND name    NE '?'.

    LOOP AT lt_cross ASSIGNING <ls_cross>.
      CASE <ls_cross>-type.

        WHEN 'B'.
          ls_wbobj_key-object = 'DYNP'.
          ls_wbobj_key-s_dynp-screen_number = <ls_cross>-prog.
          ls_wbobj_key-s_dynp-program_name = <ls_cross>-include.
          CALL METHOD lcl_convert_wbobj_key_2_e071=>execute
            EXPORTING
              is_wbobj_key = ls_wbobj_key
            CHANGING
              es_e071_key  = ls_e071_key.
          mac_collect is_e071_key 'H' '' ls_e071_key.

* CALL FUNCTION.
*   R3TR FUGR - LIMU FUNC
*   R3TR ENQU ( LOCK OBJECTS )
        WHEN 'F'.

          IF <ls_cross>-name(9) = 'ENQUEUE_E'
                  OR <ls_cross>-name(9) = 'DEQUEUE_E'.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'ENQU'.
            ls_e071_key-obj_name = <ls_cross>-name+8.
            mac_collect is_e071_key 'H' '' ls_e071_key.
          ELSE.
            ls_e071_key-pgmid    = 'LIMU'.
            ls_e071_key-object   = 'FUNC'.
            ls_e071_key-obj_name = <ls_cross>-name.
            mac_collect is_e071_key 'H' '' ls_e071_key.
          ENDIF.

* TYPE-POOLS
        WHEN 'G'.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'TYPE'.
          ls_e071_key-obj_name = <ls_cross>-name.
          mac_collect is_e071_key 'H' '' ls_e071_key.

* MESSAGE NUMBER
*   R3TR MSAG
*   LIMU MESS
        WHEN 'N'.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'MSAG'.
          ls_e071_key-obj_name = <ls_cross>-name(20).
          mac_collect is_e071_key 'H' '' ls_e071_key.
          IF NOT <ls_cross>-name+20(3) IS INITIAL.
            ls_wbobj_key-object = 'MESS'.
            ls_wbobj_key-s_mess-msg_number = <ls_cross>-name+20(3).
            ls_wbobj_key-s_mess-msg_class_name = <ls_cross>-name(20).
            CALL METHOD lcl_convert_wbobj_key_2_e071=>execute
              EXPORTING
                is_wbobj_key = ls_wbobj_key
              CHANGING
                es_e071_key  = ls_e071_key.
            mac_collect is_e071_key 'H' '' ls_e071_key.
          ENDIF.

* SEARCH HELP (M = in dynpro, V = in program).
*   R3TR SHLD or R3TR SHLP
        WHEN 'M' OR 'V'.
          ls_e071_key-object   = 'SHLP'.
          ls_e071_key-obj_name = <ls_cross>-name.
          mac_collect is_e071_key 'H' '' ls_e071_key.

* GET PARAMETER or SET PARAMETER
        WHEN 'P'.
          ls_e071_key-object   = 'PARA'.
          ls_e071_key-obj_name = <ls_cross>-name.
          mac_collect is_e071_key 'H' '' ls_e071_key.

* SUBMIT
        WHEN 'R'.
          ls_e071_key-object   = 'PROG'.
          ls_e071_key-obj_name = <ls_cross>-name.
          mac_collect is_e071_key 'H' '' ls_e071_key.

*
        WHEN 'S'.
          ls_e071_key-object   = 'TABL'.
          ls_e071_key-obj_name = <ls_cross>-name.
          mac_collect is_e071_key 'H' '' ls_e071_key.

* CALL TRANSACTION
        WHEN 'T'.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'TRAN'.
          ls_e071_key-obj_name = <ls_cross>-name.
          mac_collect is_e071_key 'H' '' ls_e071_key.

* PERFORM cross-name IN PROGRAM cross-prog
        WHEN 'U'.
          ls_e071_key-object   = 'PROG'.
          ls_e071_key-obj_name = <ls_cross>-prog.
          mac_collect is_e071_key 'H' '' ls_e071_key.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.                    "get_include_required_objects

  METHOD get_reqobj.
    DATA ls_e071_key TYPE type_s_e071_objkey.
    DATA ls_wbobj_key TYPE  lcl_convert_wbobj_key_2_e071=>type_s_wbobj_key.
    DATA lt_dd03p TYPE TABLE OF dd03p.
    FIELD-SYMBOLS <ls_dd03p> TYPE dd03p.
    DATA lt_dd08v TYPE TABLE OF dd08v.
    FIELD-SYMBOLS <ls_dd08v> TYPE dd08v.
    DATA lt_dd35v TYPE TABLE OF dd35v.
    FIELD-SYMBOLS <ls_dd35v> TYPE dd35v.
    DATA l_domname TYPE ddobjname.
    DATA l_rollname TYPE ddobjname.
    DATA l_devclass TYPE tadir-devclass.
    DATA l_progname TYPE syrepid.
    DATA l_include TYPE trdir-name.
    DATA l_subc TYPE trdir-subc.
    DATA l_lockobject TYPE ddobjname.
    DATA ls_dd01v TYPE dd01v.
    DATA ls_dd04v TYPE dd04v.
    DATA ls_dd30v TYPE dd30v.
    DATA l_shlpname TYPE ddobjname.
    DATA l_object TYPE e071-object.
    DATA ls_dd25v TYPE dd25v.
    DATA ls_e071 TYPE e071.
    DATA ls_tadir TYPE tadir.
    DATA l_ddobjname TYPE ddobjname.
    DATA lt_dd26v TYPE TABLE OF dd26v.
    DATA ls_dd26v TYPE dd26v.

    ls_e071_key = is_e071_key.

    REFRESH et_e071_key.
    REFRESH gt_e071_key.

    DATA ls_ko100 TYPE ko100.
    CALL FUNCTION 'TR_GET_PGMID_FOR_OBJECT'
      EXPORTING
        iv_object      = ls_e071_key-object
      IMPORTING
        es_type        = ls_ko100
      EXCEPTIONS
        illegal_object = 1
        OTHERS         = 2.
    IF sy-subrc = 0.
      ls_e071_key-pgmid = ls_ko100-pgmid.
    ENDIF.


    MOVE-CORRESPONDING is_e071_key TO ls_e071.
    CALL METHOD lcl_sap_fm_tr_check_type=>execute
      EXPORTING
        wi_e071  = ls_e071
      IMPORTING
        we_tadir = ls_tadir.

    SELECT SINGLE devclass FROM tadir INTO l_devclass
          WHERE pgmid     = ls_tadir-pgmid
            AND object    = ls_tadir-object
            AND obj_name  = ls_tadir-obj_name.
    IF sy-subrc = 0.
      ls_e071_key-pgmid    = 'R3TR'.
      ls_e071_key-object   = 'DEVC'.
      ls_e071_key-obj_name = l_devclass.
      mac_collect is_e071_key 'S' '' ls_e071_key.
    ENDIF.

* Add main program
*  LIMU METH YYYY  XXXX -> Add R3TR CLAS YYYY
*  LIMU DYNP YYYY     XXXX -> Add R3TR PROG YYYY
*  LIMU DYNP SAPLYYYY XXXX -> Add R3TR FUGR YYYY
*  LIMU MESS YYYY  XXX -> Add R3TR MSAG YYYY
* etc.
    IF is_e071_key-pgmid = 'LIMU'.
      ls_e071_key-pgmid    = ls_tadir-pgmid.
      ls_e071_key-object   = ls_tadir-object.
      ls_e071_key-obj_name = ls_tadir-obj_name.
      mac_collect is_e071_key 'H' '' ls_e071_key.
    ENDIF.

    ls_e071_key = is_e071_key.

    CASE ls_e071_key-object.
*---------------------
* DEVELOPMENT CLASS
*---------------------
      WHEN 'DEVC'.
        DATA l_package_name TYPE devclass.
        l_package_name = is_e071_key-obj_name.
        CALL METHOD get_devc_required_objects
          EXPORTING
            i_package_name = l_package_name.

*---------------------
* transaction code
*---------------------
      WHEN 'TRAN'.

        DATA l_tcode TYPE tcode.
        DATA lo_transaction TYPE REF TO lcl_tran.

        l_tcode = is_e071_key-obj_name.
        CREATE OBJECT lo_transaction
          EXPORTING
            i_tcode = l_tcode.

        CASE lo_transaction->type.
          WHEN 'O'.
            IF lo_transaction->s_object-local_class = 'X'.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'PROG'.
              ls_e071_key-obj_name = lo_transaction->s_object-program_name.
              mac_collect is_e071_key 'H' '' ls_e071_key.
            ELSE.
              ls_e071_key-pgmid   = 'LIMU'.
              ls_wbobj_key-object = 'METH'.
              ls_wbobj_key-s_meth-method_name = lo_transaction->s_object-method_name.
              ls_wbobj_key-s_meth-class_name = lo_transaction->s_object-global_class_name.
              CALL METHOD lcl_convert_wbobj_key_2_e071=>execute
                EXPORTING
                  is_wbobj_key = ls_wbobj_key
                CHANGING
                  es_e071_key  = ls_e071_key.
              mac_collect is_e071_key 'H' '' ls_e071_key.
            ENDIF.
            IF NOT lo_transaction->s_object-auth_object IS INITIAL.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'SUSO'.
              ls_e071_key-obj_name = lo_transaction->s_object-auth_object.
              mac_collect is_e071_key 'H' '' ls_e071_key.
            ENDIF.
          WHEN 'D'.
            ls_e071_key-pgmid = 'LIMU'.
            ls_wbobj_key-object = 'DYNP'.
            ls_wbobj_key-s_dynp-program_name = lo_transaction->s_dialog-program_name.
            ls_wbobj_key-s_dynp-screen_number = lo_transaction->s_dialog-screen_number.
            CALL METHOD lcl_convert_wbobj_key_2_e071=>execute
              EXPORTING
                is_wbobj_key = ls_wbobj_key
              CHANGING
                es_e071_key  = ls_e071_key.
            mac_collect is_e071_key 'H' '' ls_e071_key.
            IF NOT lo_transaction->s_dialog-auth_object IS INITIAL.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'SUSO'.
              ls_e071_key-obj_name = lo_transaction->s_dialog-auth_object.
              mac_collect is_e071_key 'H' '' ls_e071_key.
            ENDIF.
          WHEN 'P'.
            IF NOT lo_transaction->s_parameter-called_tcode IS INITIAL.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'TRAN'.
              ls_e071_key-obj_name = lo_transaction->s_parameter-called_tcode.
              mac_collect is_e071_key 'H' '' ls_e071_key.
            ENDIF.
            IF NOT lo_transaction->s_parameter-program_name IS INITIAL.
              ls_e071_key-pgmid   = 'LIMU'.
              ls_wbobj_key-object = 'DYNP'.
              ls_wbobj_key-s_dynp-program_name = lo_transaction->s_parameter-program_name.
              ls_wbobj_key-s_dynp-screen_number = lo_transaction->s_parameter-screen_number.
              CALL METHOD lcl_convert_wbobj_key_2_e071=>execute
                EXPORTING
                  is_wbobj_key = ls_wbobj_key
                CHANGING
                  es_e071_key  = ls_e071_key.
              mac_collect is_e071_key 'H' '' ls_e071_key.
            ENDIF.
          WHEN 'V'.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'TRAN'.
            ls_e071_key-obj_name = lo_transaction->s_variant-called_tcode.
            mac_collect is_e071_key 'H' '' ls_e071_key.
            IF NOT lo_transaction->s_variant-transac_variant IS INITIAL.
              ls_e071_key-object   = 'STVI'.
              ls_e071_key-obj_name = lo_transaction->s_variant-transac_variant.
              mac_collect is_e071_key 'H' '' ls_e071_key.
            ENDIF.
          WHEN 'R'.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'PROG'.
            ls_e071_key-obj_name = lo_transaction->s_report-program_name.
            mac_collect is_e071_key 'H' '' ls_e071_key.
            IF NOT lo_transaction->s_report-auth_object IS INITIAL.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'SUSO'.
              ls_e071_key-obj_name = lo_transaction->s_report-auth_object.
              mac_collect is_e071_key 'H' '' ls_e071_key.
            ENDIF.
            IF NOT lo_transaction->s_report-program_variant IS INITIAL.
              ls_e071_key-pgmid   = 'LIMU'.
* TODO : could be also VARX (&CUS...)
              ls_wbobj_key-object = 'VARI'.
              ls_wbobj_key-s_vari-program_name = lo_transaction->s_report-program_name.
              ls_wbobj_key-s_vari-variant_name = lo_transaction->s_report-program_variant.
              CALL METHOD lcl_convert_wbobj_key_2_e071=>execute
                EXPORTING
                  is_wbobj_key = ls_wbobj_key
                CHANGING
                  es_e071_key  = ls_e071_key.
              mac_collect is_e071_key 'H' '' ls_e071_key.
            ENDIF.
        ENDCASE.

*---------------------
* ENHANCEMENT Implementation
*---------------------
      WHEN 'ENHO'.
        DATA lo_enh TYPE REF TO if_enh_tool.
        DATA lo_enh2 TYPE REF TO cl_enh_tool_hook_impl.
        DATA lo_enh99 TYPE REF TO cl_enh_tool_clif.
        DATA l_enhname TYPE enhname.
        DATA lt_enhobj TYPE TABLE OF enhobj.
        DATA ls_enhobj TYPE enhobj.
        DATA l_tool_type TYPE enhtooltype.

        l_enhname = is_e071_key-obj_name.

        lo_enh = cl_enh_factory=>get_enhancement( enhancement_id = l_enhname ).
        l_tool_type = lo_enh->get_tool( ).

* For a badi, we'll get ENHS, CLAS, INTF.
* For a hook, we'll get the main program (FUGR or PROG or CLAS)
* For a class enh, we'll get the class name (+ its interfaces + its
*   superclasses up to top class) and the enhanced methods (new parameters)
* For a function group enh, we'll get the function group and the enhanced
*   function module (new parameters)
        SELECT * FROM enhobj INTO TABLE lt_enhobj WHERE enhname = l_enhname.
        LOOP AT lt_enhobj INTO ls_enhobj.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = ls_enhobj-obj_type.
          ls_e071_key-obj_name = ls_enhobj-obj_name.
          mac_collect is_e071_key 'H' '' ls_e071_key.
        ENDLOOP.

        CASE l_tool_type.
****
          WHEN 'HOOK_IMPL'.
            lo_enh2 ?= lo_enh.
            CALL METHOD lo_enh2->get_hook_impls_include
              IMPORTING
                include = l_include.
            CALL METHOD get_include_required_objects
              EXPORTING
                i_include = l_include.

****
          WHEN 'CLASENH'.
            lo_enh99 ?= lo_enh.
            CALL METHOD lo_enh99->get_include_name
              IMPORTING
                include = l_include.
            CALL METHOD get_include_required_objects
              EXPORTING
                i_include = l_include.
****
          WHEN 'FUGRENH'.
            lo_enh99 ?= lo_enh.
            CALL METHOD lo_enh99->get_include_name
              IMPORTING
                include = l_include.
            CALL METHOD get_include_required_objects
              EXPORTING
                i_include = l_include.
**** TODO
          WHEN 'BADI_IMPL'.
*            lo_enh5 ?= lo_enh.
*          DATA lt_impl TYPE enh_badi_impl_data_it.
*          DATA ls_impl TYPE LINE OF enh_badi_impl_data_it.
*          lt_impl = lo_enh5->get_implementations( ).
*          LOOP AT lt_impl INTO ls_impl.
**            ls_e071_key-pgmid    = 'R3TR'.
**            ls_e071_key-object   = 'SXCI'.
**            ls_e071_key-obj_name = ls_impl-impl_name.
**            mac_collect is_e071_key 'H' '' ls_e071_key.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'INTF'.
*            CONCATENATE 'IF_EX_' ls_impl-impl_name INTO ls_e071_key-obj_name.
*            mac_collect is_e071_key 'H' '' ls_e071_key.
*            ls_e071_key-pgmid    = 'R3TR'.
*            ls_e071_key-object   = 'CLAS'.
*            ls_e071_key-obj_name = ls_impl-impl_class.
*            mac_collect is_e071_key 'H' '' ls_e071_key.
*          ENDLOOP.

**** TODO
          WHEN 'INTFENH'.
*            lo_enh6 ?= lo_enh.

**** TODO
          WHEN 'WDYENH'.
*            lo_enh7 ?= lo_enh.
        ENDCASE.

*---------------------
* PROGRAM
*---------------------
      WHEN 'PROG' OR 'REPS' OR 'REPO'.
        CASE ls_e071_key-object.
          WHEN 'REPS' OR 'REPO'.
            SELECT SINGLE subc FROM trdir INTO l_subc WHERE name = l_progname.
            CASE l_subc.
              WHEN '1' OR 'M' OR 'F' OR 'S' OR 'K'.
                ls_e071_key-pgmid    = 'LIMU'.
                ls_e071_key-object   = 'REPT'.
                ls_e071_key-obj_name = is_e071_key-obj_name.
                mac_collect is_e071_key 'H' '' ls_e071_key.
            ENDCASE.
        ENDCASE.
        l_include = is_e071_key-obj_name.
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = l_include.

*---------------------
* interface
*---------------------
      WHEN 'INTF' OR 'INTD'.
        DATA l_interface_name TYPE seoclass-clsname.
        l_interface_name = is_e071_key-obj_name.
        CALL FUNCTION 'RS_PROGNAME_CONCATENATE'
          EXPORTING
            intf_name       = l_interface_name
          IMPORTING
            intf_progname   = l_include
          EXCEPTIONS
            delimiter_error = 1
            OTHERS          = 2.
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = l_include.

*---------------------
* class
*---------------------
      WHEN 'CLAS'.
        DATA l_class_name TYPE seoclass-clsname.
        l_class_name = is_e071_key-obj_name.
        CALL FUNCTION 'RS_PROGNAME_CONCATENATE'
          EXPORTING
            clas_name       = l_class_name
          IMPORTING
            clas_progname   = l_include
          EXCEPTIONS
            delimiter_error = 1
            OTHERS          = 2.
        IF sy-subrc = 0.
          CALL METHOD get_include_required_objects
            EXPORTING
              i_include = l_include.
        ENDIF.

*---------------------
* method
*---------------------
      WHEN 'METH'.
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = ls_wbobj_key-include.

*---------------------
* function
*---------------------
      WHEN 'FUNC'.
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = ls_wbobj_key-include.

*---------------------
* TYPE-POOL
*---------------------
      WHEN 'TYPE' OR 'TYPD'.
        DATA l_type_name TYPE trdir-name.
        l_type_name = is_e071_key-obj_name.
        CALL FUNCTION 'RS_PROGNAME_CONCATENATE'
          EXPORTING
            type_name       = l_type_name
          IMPORTING
            type_progname   = l_include
          EXCEPTIONS
            delimiter_error = 1
            OTHERS          = 2.
        CALL METHOD get_include_required_objects
          EXPORTING
            i_include = l_include.

*---------------------
* FUNCTION GROUP
*---------------------
      WHEN 'FUGT'.
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'FUGR'.
        ls_e071_key-obj_name = is_e071_key-obj_name.
        mac_collect is_e071_key 'H' '' ls_e071_key.
      WHEN 'FUGR'.
        DATA l_function_group TYPE rs38l-area.
        l_function_group = is_e071_key-obj_name.
        CALL FUNCTION 'RS_PROGNAME_CONCATENATE'
          EXPORTING
            fugr_group          = l_function_group
          IMPORTING
            fugr_progname_group = l_include
          EXCEPTIONS
            delimiter_error     = 1
            OTHERS              = 2.
        IF sy-subrc = 0.
          CALL METHOD get_include_required_objects
            EXPORTING
              i_include = l_include.
        ENDIF.

*---------------------
* TABLE or STRUCTURE
*---------------------
      WHEN 'TABT'.
        ls_e071_key-pgmid    = 'R3TR'.
        ls_e071_key-object   = 'TABL'.
        ls_e071_key-obj_name = is_e071_key-obj_name.
        mac_collect is_e071_key 'H' '' ls_e071_key.
      WHEN 'TABL' OR 'TABD'.
        DATA l_name TYPE ddobjname.
        l_name = is_e071_key-obj_name.
        CALL FUNCTION 'DDIF_TABL_GET'
          EXPORTING
            name          = l_name
          TABLES
            dd03p_tab     = lt_dd03p
            dd08v_tab     = lt_dd08v
            dd35v_tab     = lt_dd35v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
* Fields : data elements, domaines, tables de contrôles
          LOOP AT lt_dd03p ASSIGNING <ls_dd03p>.
            CALL METHOD tablstruc_field
              EXPORTING
                is_dd03p    = <ls_dd03p>
                is_e071_key = is_e071_key.

            IF NOT <ls_dd03p>-checktable IS INITIAL.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'TABL'.
              ls_e071_key-obj_name = <ls_dd03p>-checktable.
              mac_collect is_e071_key 'H' '' ls_e071_key.
            ENDIF.
          ENDLOOP.
* Foreign keys
          LOOP AT lt_dd08v ASSIGNING <ls_dd08v>.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'TABL'.
            ls_e071_key-obj_name = <ls_dd08v>-checktable.
            mac_collect is_e071_key 'H' '' ls_e071_key.
          ENDLOOP.
* Search helps
          LOOP AT lt_dd35v ASSIGNING <ls_dd35v>.
            IF NOT <ls_dd35v>-shlpname IS INITIAL.
              ls_e071_key-pgmid    = 'R3TR'.
              ls_e071_key-object   = 'SHLP'.
              ls_e071_key-obj_name = <ls_dd35v>-shlpname.
              mac_collect is_e071_key 'H' '' ls_e071_key.
            ENDIF.
          ENDLOOP.
        ENDIF.

*---------------------
* VIEW
*---------------------
      WHEN 'VIEW' OR 'VIED'.
        l_lockobject = is_e071_key-obj_name.
        CALL FUNCTION 'DDIF_VIEW_GET'
          EXPORTING
            name          = l_ddobjname
          IMPORTING
            dd25v_wa      = ls_dd25v
          TABLES
            dd26v_tab     = lt_dd26v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          LOOP AT lt_dd26v INTO ls_dd26v.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'TABL'.
            ls_e071_key-obj_name = ls_dd26v-tabname.
            mac_collect is_e071_key 'H' '' ls_e071_key.
          ENDLOOP.
        ENDIF.

*---------------------
* TABLE TYPE
*---------------------
      WHEN 'TTYP' OR 'TTYD'.
        l_ddobjname = is_e071_key-obj_name.
        DATA ls_dd40v TYPE dd40v.
        CALL FUNCTION 'DDIF_TTYP_GET'
          EXPORTING
            name          = l_ddobjname
          IMPORTING
            dd40v_wa      = ls_dd40v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          ls_e071_key-pgmid    = 'R3TR'.
          CALL METHOD lcl_sap_fm_ddif_typeinfo_get=>execute
            EXPORTING
              i_tabname = ls_dd40v-rowtype
            CHANGING
              e_object  = ls_e071_key-object.
          ls_e071_key-obj_name = ls_dd40v-rowtype.
          mac_collect is_e071_key 'H' '' ls_e071_key.
        ENDIF.

*---------------------
* DATA ELEMENT
*---------------------
      WHEN 'DTEL' OR 'DTED'.
        l_rollname = is_e071_key-obj_name.
        DATA ls_tpara TYPE tpara.
        CALL FUNCTION 'DDIF_DTEL_GET'
          EXPORTING
            name          = l_rollname
          IMPORTING
            dd04v_wa      = ls_dd04v
            tpara_wa      = ls_tpara
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          IF NOT ls_dd04v-domname IS INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'DOMA'.
            ls_e071_key-obj_name = ls_dd04v-domname.
            mac_collect is_e071_key 'H' '' ls_e071_key.
          ENDIF.
          IF NOT ls_dd04v-shlpname IS INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'SHLP'.
            ls_e071_key-obj_name = ls_dd04v-shlpname.
            mac_collect is_e071_key 'H' '' ls_e071_key.
          ENDIF.
          IF NOT ls_tpara-paramid IS INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'PARA'.
            ls_e071_key-obj_name = ls_tpara-paramid.
            mac_collect is_e071_key 'H' '' ls_e071_key.
          ENDIF.
        ENDIF.

*---------------------
* DOMAIN
*---------------------
      WHEN 'DOMA' OR 'DOMD'.
        l_domname = is_e071_key-obj_name.
        CALL FUNCTION 'DDIF_DOMA_GET'
          EXPORTING
            name          = l_domname
          IMPORTING
            dd01v_wa      = ls_dd01v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          IF NOT ls_dd01v-entitytab IS INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'TABL'.
            ls_e071_key-obj_name = ls_dd01v-entitytab.
            mac_collect is_e071_key 'H' '' ls_e071_key.
          ENDIF.
        ENDIF.

*---------------------
* SEARCH HELP
*---------------------
      WHEN 'SHLP' OR 'SHLD'.
        l_shlpname = is_e071_key-obj_name.
        CALL FUNCTION 'DDIF_SHLP_GET'
          EXPORTING
            name          = l_shlpname
          IMPORTING
            dd30v_wa      = ls_dd30v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          IF NOT ls_dd30v-selmethod IS INITIAL.
            CALL METHOD lcl_sap_fm_ddif_typeinfo_get=>execute
              EXPORTING
                i_tabname = ls_dd30v-selmethod
              CHANGING
                e_object  = l_object.
            ls_e071_key-object   = l_object.
            ls_e071_key-obj_name = ls_dd30v-selmethod.
            mac_collect is_e071_key 'H' '' ls_e071_key.
          ENDIF.
          IF NOT ls_dd30v-texttab IS INITIAL.
            ls_e071_key-pgmid    = 'R3TR'.
            ls_e071_key-object   = 'TABL'.
            ls_e071_key-obj_name = ls_dd30v-texttab.
            mac_collect is_e071_key 'H' '' ls_e071_key.
          ENDIF.
        ENDIF.

*---------------------
* LOCK OBJECT
*---------------------
      WHEN 'ENQU'.
        l_lockobject = is_e071_key-obj_name.
        CALL FUNCTION 'DDIF_ENQU_GET'
          EXPORTING
            name          = l_lockobject
          IMPORTING
            dd25v_wa      = ls_dd25v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          ls_e071_key-pgmid    = 'R3TR'.
          ls_e071_key-object   = 'TABL'.
          ls_e071_key-obj_name = ls_dd25v-roottab.
          mac_collect is_e071_key 'H' '' ls_e071_key.
        ENDIF.

    ENDCASE.

    et_e071_key = gt_e071_key.

  ENDMETHOD.                    "get_obj

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
  METHOD tablstruc_field.
    DATA ls_e071_key TYPE type_s_e071_objkey.

    IF is_dd03p-fieldname = '.APPEND_DU'.
* .APPEND_DU indicates a recurse structure, PRECFIELD is empty, ignore it

    ELSEIF is_dd03p-fieldname = '.APPEND'
          OR is_dd03p-fieldname(6) = '.INCLU'.
* .INCLU-*** indicates include with suffix *** for components
      ls_e071_key-pgmid    = 'R3TR'.
      ls_e071_key-object   = 'TABL'.
      ls_e071_key-obj_name = is_dd03p-precfield.
      IF is_dd03p-precfield CP 'CI_*'
            OR is_dd03p-precfield CP 'SI_*'.
* activation won't fail if the CI_* include doesn't exist
        mac_collect is_e071_key 'S' '' ls_e071_key.
      ELSE.
* activation will fail if other includes don't exist
        mac_collect is_e071_key 'H' '' ls_e071_key.
      ENDIF.
* components with intern type / length have COMPTYPE = blank
* (don't test ROLLNAME which sometimes is not blank)
    ELSEIF NOT is_dd03p-comptype IS INITIAL.
      ls_e071_key-pgmid    = 'R3TR'.
      DATA l_link TYPE c LENGTH 1.
      l_link = 'H'.
      CASE is_dd03p-comptype.
        WHEN 'E'.
          ls_e071_key-object   = 'DTEL'.
        WHEN 'S'.
          ls_e071_key-object   = 'TABL'.
        WHEN 'L'.
          ls_e071_key-object   = 'TTYP'.
        WHEN 'R'. "type ref to
          CASE is_dd03p-reftype.
            WHEN 'C'.
              ls_e071_key-object   = 'CLAS'.
            WHEN 'I'.
              ls_e071_key-object   = 'INTF'.
            WHEN 'E'.
              ls_e071_key-object   = 'DTEL'.
            WHEN 'S'. "structure or table
              ls_e071_key-object   = 'TABL'.
            WHEN 'L'.
              ls_e071_key-object   = 'TTYP'.
            WHEN space. "undefined
              DATA l_typename TYPE typename.
              DATA l_typekind TYPE ddtypekind.
              l_typename = is_dd03p-rollname.
              CALL FUNCTION 'DDIF_TYPEINFO_GET'
                EXPORTING
                  typename = l_typename
                IMPORTING
                  typekind = l_typekind.
              IF l_typekind IS NOT INITIAL.
                ls_e071_key-object   = l_typekind.
              ENDIF.
            WHEN 'B'.
* reference to an internal data type, length, decimals -> IGNORE
            WHEN OTHERS.
              ASSERT is_dd03p-reftype = 'D' "data
                    OR is_dd03p-reftype = 'O'. "object
          ENDCASE.
        WHEN 'N'. "non existing structure (or inactive), i.e. CI_* or SI_*
          ls_e071_key-object = 'TABL'.
          l_link = 'S'. "include ci_* may not exist
        WHEN OTHERS.
          MESSAGE x001(00).
      ENDCASE.
      IF ls_e071_key-object IS NOT INITIAL.
        ls_e071_key-obj_name = is_dd03p-rollname.
        mac_collect is_e071_key l_link '' ls_e071_key.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "tablstruc_field


ENDCLASS.                    "lcl_dev_cross_ref IMPLEMENTATION
