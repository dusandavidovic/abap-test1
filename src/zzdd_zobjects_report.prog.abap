**---------------------------------------------------------------------*
** Report  ZZDD_ZOBJECTS_REPORT
**
**---------------------------------------------------------------------*
**
**
**---------------------------------------------------------------------*

REPORT  zzdd_zobjects_report NO STANDARD PAGE HEADING.

TABLES:tadir,tstc,v_username,vrsd.

TYPE-POOLS:slis,vrm.
TYPES: BEGIN OF ittemp,
       object    LIKE tadir-object,
       obj_name  LIKE tadir-obj_name,
       text      LIKE trdirt-text,
       author    LIKE tadir-author,
       devclass  LIKE tadir-devclass,
       name_text LIKE v_username-name_text,
       tcode LIKE tstc-tcode,
       korrnum LIKE vrsd-korrnum,
       END OF ittemp.

DATA: itfinal TYPE STANDARD TABLE OF ittemp WITH HEADER LINE,
      wafinal   TYPE ittemp.

DATA : name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.


DATA:itfieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA:itrepid TYPE sy-repid.
itrepid = sy-repid.
DATA:itevent TYPE slis_t_event.
DATA:itlistheader TYPE slis_t_listheader.
DATA:walistheader LIKE LINE OF itlistheader.
DATA:itlayout TYPE slis_layout_alv.
DATA:top TYPE slis_formname.
DATA:itsort TYPE slis_t_sortinfo_alv WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETER: package LIKE tadir-devclass.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.


  PERFORM getdata.
  PERFORM alv.
*----
**      Form  GETDATA
*----
*      text
*----
FORM getdata.
*read the repository object table and link with username if found
  SELECT  tadir~object
          tadir~obj_name
          trdirt~text
          tadir~author
          tadir~devclass
          v_username~name_text
          INTO TABLE itfinal
          FROM tadir
          LEFT JOIN v_username
          ON tadir~author = v_username~bname
          LEFT JOIN trdirt
          ON tadir~obj_name = trdirt~name
          WHERE tadir~devclass = package.   "'$TMP'
*          AND ( tadir~obj_name LIKE 'Z%' OR tadir~obj_name LIKE 'Y%' ).

  CHECK sy-subrc EQ 0.
  LOOP AT itfinal.

*TCODE FROM TSTC
    SELECT SINGLE tcode FROM tstc INTO (itfinal-tcode) WHERE pgmna =
    itfinal-obj_name.

*LATEST TRANSPORT REQUEST NUMBER FROM VRSD
    SELECT SINGLE korrnum FROM vrsd INTO (itfinal-korrnum) WHERE objname =
    itfinal-obj_name.
    MODIFY itfinal.
  ENDLOOP.
*  DELETE itfinal WHERE korrnum IS INITIAL.
  SORT itfinal BY author object.
ENDFORM.                    "GETDATA

*----
**      Form  ALV
*----
*      text
*----
FORM alv.
  IF itfinal[] IS INITIAL.
    MESSAGE 'No Values exist for the Selection.' TYPE 'S'.
    STOP.
  ENDIF.


  DEFINE m_fieldcat.
    itfieldcat-fieldname = &1.
    itfieldcat-col_pos = &2.
    itfieldcat-seltext_l = &3.
    itfieldcat-do_sum = &4.
    itfieldcat-outputlen = &5.
    append itfieldcat to itfieldcat.
    clear itfieldcat.
  END-OF-DEFINITION.

  m_fieldcat 'OBJECT' ''   'OBJECT' ''       04  .
  m_fieldcat 'OBJ_NAME' '' 'PROGRAM NAME' '' 40 .
  m_fieldcat 'TCODE' ''    'TCODE' ''        20 .
  m_fieldcat 'TEXT' ''     'DESCRIPTION' ''  70 .
  m_fieldcat 'AUTHOR' ''   'AUTHOR' ''       80 .
  m_fieldcat 'DEVCLASS' '' 'PACKAGE' ''      30 .
  m_fieldcat 'KORRNUM' ''  'LATEST TRANSPORT REQUEST' '' 20 .

  itlayout-zebra = 'X'.
  itlayout-colwidth_optimize = 'X'.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      is_layout               = itlayout
      i_callback_user_command = 'LIST1'
      i_callback_top_of_page  = 'TOP'
      it_fieldcat             = itfieldcat[]
      i_save                  = 'A'
*      is_variant              = itvariant
      it_events               = itevent[]
*      is_print                = itprintparams
      it_sort                 = itsort[]
    TABLES
      t_outtab                = itfinal
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "ALV
*----
**      Form  TOP
*----
*    Top of page for ALV Report
*----
FORM top.
  DATA:string1(70),
       string2(70),
       title1(100),
       title2(100),
       count(10).

  DESCRIBE TABLE itfinal LINES count.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = itevent
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  string1 = 'List of Objects in Development Class'.
  CONCATENATE string1 ':' itfinal-devclass INTO title1.
  walistheader-typ = 'H'.
  walistheader-info = title1.
  APPEND walistheader TO itlistheader.

  string2 = 'Total No.of Objects'.
  CONCATENATE string2 ':' count INTO title2.
  walistheader-typ = 'H'.
  walistheader-info = title2.
  APPEND walistheader TO itlistheader.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = itlistheader
      i_logo             = ''.
*  i_end_of_list_grid       =        .
*ENDIF.
  CLEAR itlistheader.
*ENDIF.
ENDFORM.                    "TOP

*----
**      Form  list1
*----
*      ALV Interactive-
*----
*     -->R_UCOMM    text
*     -->RS_SELFIELDtext
*----
FORM list1 USING r_ucomm LIKE sy-ucomm rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '*IC1'.

      IF rs_selfield-fieldname = 'OBJ_NAME'.
        READ TABLE itfinal INDEX rs_selfield-tabindex.
        SET PARAMETER ID 'RID' FIELD itfinal-obj_name.
        CALL TRANSACTION 'SE38' AND SKIP FIRST SCREEN.
      ELSEIF rs_selfield-fieldname = 'TCODE'.
        READ TABLE itfinal INDEX rs_selfield-tabindex.
        SET PARAMETER ID 'TCD' FIELD itfinal-tcode.
        CALL TRANSACTION 'SESSION_MANAGER' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
ENDFORM.                                                    "list1
