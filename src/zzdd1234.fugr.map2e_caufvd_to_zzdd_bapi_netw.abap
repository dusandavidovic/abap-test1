FUNCTION MAP2E_CAUFVD_TO_ZZDD_BAPI_NETW.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(CAUFVD) LIKE  CAUFVD STRUCTURE  CAUFVD
*"  CHANGING
*"     REFERENCE(ZZDD_BAPI_NETWORK_EXP) LIKE  ZZDD_BAPI_NETWORK_EXP
*"  STRUCTURE  ZZDD_BAPI_NETWORK_EXP
*"  EXCEPTIONS
*"      ERROR_CONVERTING_KEYS
*"--------------------------------------------------------------------

* This function module was generated. Don't change it manually!
* <VERSION>|5
* <BAPI_STRUCTURE>|ZZDD_BAPI_NETWORK_EXP
* <SAP_STRUCTURE>|CAUFVD
* <INTERN_TO_EXTERN>|X
* <APPEND FORM>|

* <BAPI_FIELD>|FUNC_AREA_LONG
* <SAP_FIELD>|FUNC_AREA
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-FUNC_AREA
  TO ZZDD_BAPI_NETWORK_EXP-FUNC_AREA_LONG                         .

* <BAPI_FIELD>|ACTUAL_RELEASE_DATE
* <SAP_FIELD>|FTRMI
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-FTRMI
  TO ZZDD_BAPI_NETWORK_EXP-ACTUAL_RELEASE_DATE                    .

* <BAPI_FIELD>|CONFIRMED_FINISH_DATE
* <SAP_FIELD>|GETRI
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-GETRI
  TO ZZDD_BAPI_NETWORK_EXP-CONFIRMED_FINISH_DATE                  .

* <BAPI_FIELD>|ACTUAL_START_DATE
* <SAP_FIELD>|GSTRI
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-GSTRI
  TO ZZDD_BAPI_NETWORK_EXP-ACTUAL_START_DATE                      .

* <BAPI_FIELD>|SCHED_RELEASE_DATE_FORECAST
* <SAP_FIELD>|FTRPS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-FTRPS
  TO ZZDD_BAPI_NETWORK_EXP-SCHED_RELEASE_DATE_FORECAST            .

* <BAPI_FIELD>|SCHED_FINISH_DATE_FORECAST
* <SAP_FIELD>|GLTPS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-GLTPS
  TO ZZDD_BAPI_NETWORK_EXP-SCHED_FINISH_DATE_FORECAST             .

* <BAPI_FIELD>|SCHED_START_DATE_FORECAST
* <SAP_FIELD>|GSTPS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-GSTPS
  TO ZZDD_BAPI_NETWORK_EXP-SCHED_START_DATE_FORECAST              .

* <BAPI_FIELD>|FINISH_DATE_FORECAST
* <SAP_FIELD>|GLTPP
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-GLTPP
  TO ZZDD_BAPI_NETWORK_EXP-FINISH_DATE_FORECAST                   .

* <BAPI_FIELD>|START_DATE_FORECAST
* <SAP_FIELD>|GSTPP
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-GSTPP
  TO ZZDD_BAPI_NETWORK_EXP-START_DATE_FORECAST                    .

* <BAPI_FIELD>|SCHED_TYPE_FORECAST
* <SAP_FIELD>|TRKZP
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-TRKZP
  TO ZZDD_BAPI_NETWORK_EXP-SCHED_TYPE_FORECAST                    .

* <BAPI_FIELD>|SCHED_RELEASE_DATE
* <SAP_FIELD>|FTRMS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-FTRMS
  TO ZZDD_BAPI_NETWORK_EXP-SCHED_RELEASE_DATE                     .

* <BAPI_FIELD>|SCHED_FINISH_DATE
* <SAP_FIELD>|GLTRS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-GLTRS
  TO ZZDD_BAPI_NETWORK_EXP-SCHED_FINISH_DATE                      .

* <BAPI_FIELD>|SCHED_START_DATE
* <SAP_FIELD>|GSTRS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-GSTRS
  TO ZZDD_BAPI_NETWORK_EXP-SCHED_START_DATE                       .

* <BAPI_FIELD>|PROFILE
* <SAP_FIELD>|PROFID
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-PROFID
  TO ZZDD_BAPI_NETWORK_EXP-PROFILE                                .

* <BAPI_FIELD>|NOT_AUTO_COSTING
* <SAP_FIELD>|NAUCOST
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-NAUCOST
  TO ZZDD_BAPI_NETWORK_EXP-NOT_AUTO_COSTING                       .

* <BAPI_FIELD>|NOT_AUTO_SCHEDULE
* <SAP_FIELD>|NAUTERM
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-NAUTERM
  TO ZZDD_BAPI_NETWORK_EXP-NOT_AUTO_SCHEDULE                      .

* <BAPI_FIELD>|PRIORITY
* <SAP_FIELD>|APRIO
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-APRIO
  TO ZZDD_BAPI_NETWORK_EXP-PRIORITY                               .

* <BAPI_FIELD>|SCHED_TYPE
* <SAP_FIELD>|TERKZ
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-TERKZ
  TO ZZDD_BAPI_NETWORK_EXP-SCHED_TYPE                             .

* <BAPI_FIELD>|MRP_CONTROLLER
* <SAP_FIELD>|DISPO
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-DISPO
  TO ZZDD_BAPI_NETWORK_EXP-MRP_CONTROLLER                         .

* <BAPI_FIELD>|START_DATE
* <SAP_FIELD>|GSTRP
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-GSTRP
  TO ZZDD_BAPI_NETWORK_EXP-START_DATE                             .

* <BAPI_FIELD>|FINISH_DATE
* <SAP_FIELD>|GLTRP
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-GLTRP
  TO ZZDD_BAPI_NETWORK_EXP-FINISH_DATE                            .

* <BAPI_FIELD>|TAXJURCODE
* <SAP_FIELD>|TXJCD
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-TXJCD
  TO ZZDD_BAPI_NETWORK_EXP-TAXJURCODE                             .

* <BAPI_FIELD>|PROFIT_CTR
* <SAP_FIELD>|PRCTR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-PRCTR
  TO ZZDD_BAPI_NETWORK_EXP-PROFIT_CTR                             .

* <BAPI_FIELD>|PLANT
* <SAP_FIELD>|WERKS
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-WERKS
  TO ZZDD_BAPI_NETWORK_EXP-PLANT                                  .

* <BAPI_FIELD>|SHORT_TEXT
* <SAP_FIELD>|KTEXT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-KTEXT
  TO ZZDD_BAPI_NETWORK_EXP-SHORT_TEXT                             .

* <BAPI_FIELD>|NETWORK_TYPE
* <SAP_FIELD>|AUART
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-AUART
  TO ZZDD_BAPI_NETWORK_EXP-NETWORK_TYPE                           .

* <BAPI_FIELD>|NETWORK
* <SAP_FIELD>|AUFNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE CAUFVD-AUFNR
  TO ZZDD_BAPI_NETWORK_EXP-NETWORK                                .

* <BAPI_FIELD>|PROJECT_DEFINITION
* <SAP_FIELD>|PRONR
* <CODE_PART>|PROJECT
* <ADD_FIELD>|
IF CAUFVD-PRONR
IS INITIAL.
  CLEAR ZZDD_BAPI_NETWORK_EXP-PROJECT_DEFINITION                     .
ELSE.
  CALL FUNCTION 'PDFNUM_INTERN_TO_EXTERN_CONV'
       EXPORTING
*           EDIT_IMP  =
            INT_NUM   =
       CAUFVD-PRONR
       IMPORTING
            EXT_NUM   =
       ZZDD_BAPI_NETWORK_EXP-PROJECT_DEFINITION
       EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.
  IF SY-SUBRC <> 0.
    MESSAGE A554(B1) WITH
    CAUFVD-PRONR
    'PRONR                         '
    RAISING ERROR_CONVERTING_KEYS.
  ENDIF.
ENDIF.

* <BAPI_FIELD>|WBS_ELEMENT
* <SAP_FIELD>|PROJN
* <CODE_PART>|PSP_ELEM
* <ADD_FIELD>|
IF CAUFVD-PROJN
IS INITIAL.
  CLEAR ZZDD_BAPI_NETWORK_EXP-WBS_ELEMENT                            .
ELSE.
  CALL FUNCTION 'PSPNUM_INTERN_TO_EXTERN_CONV'
       EXPORTING
*           EDIT_IMP  = ' '
            INT_NUM   =
       CAUFVD-PROJN
       IMPORTING
            EXT_NUM   =
       ZZDD_BAPI_NETWORK_EXP-WBS_ELEMENT
       EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.
  IF SY-SUBRC <> 0.
    MESSAGE A552(B1) WITH
    CAUFVD-PROJN
    'PROJN                         '
    RAISING ERROR_CONVERTING_KEYS.
  ENDIF.
ENDIF.





ENDFUNCTION.
