*&---------------------------------------------------------------------*
*& Report ZZDD_DEMO_HTTP_CLIENT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzdd_http_test02.

*DATA: lo_http_client TYPE REF TO if_http_client,
*      lo_rest_client TYPE REF TO cl_rest_http_client,
*      lo_request     TYPE REF TO if_rest_entity,
*      lo_response    TYPE REF TO if_rest_entity,
*      lv_url         TYPE        string,
*      lv_body        TYPE        string.
PARAMETERS: ppath type string MATCHCODE OBJECT aaa LOWER CASE.


TRY .
    DATA:
      client    TYPE REF TO if_http_client,
      host      TYPE string VALUE '172.31.37.226',
*      host      TYPE string VALUE 'sap01.oneriversoftware.com',
      service   TYPE string VALUE '8000',
      path      TYPE string VALUE '/sap/public/info',
      errortext TYPE string.


    cl_http_client=>create(
      EXPORTING
        host               = host    " Logical destination (specified in function call)
        service            = service    " Port Number
        scheme             = cl_http_client=>schemetype_http
*        sap_username       = sap_username    " ABAP System, User Logon Name
*        sap_client         = sap_client    " R/3 System, Client Number from Logon
      IMPORTING
        client             = client    " HTTP Client Abstraction
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    client->request->set_method( if_http_request=>co_request_method_get ).

    cl_http_utility=>set_request_uri(
      EXPORTING
        request = client->request    " HTTP Framework (iHTTP) HTTP Request
        uri     = ppath    " URI String (in the Form of /path?query-string)
    ).

** SEND
    client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      client->get_last_error( IMPORTING message = errortext ).
    ENDIF.

** RECEIVE
    client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4
    ).
    IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      client->get_last_error( IMPORTING message = errortext ).
    ENDIF.

    DATA: code   TYPE i, reason TYPE string.
    client->response->get_status(
      IMPORTING
        code   = code    " HTTP status code
        reason = reason    " HTTP status description
    ).

    DATA(cdata) = client->response->get_cdata( ).
    DATA(content_type) = client->response->get_content_type( ).

** CLOSE
    client->close(
      EXCEPTIONS
        http_invalid_state = 1
        OTHERS             = 2
    ).
    IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      client->get_last_error( IMPORTING message = errortext ).
    ENDIF.


    cl_demo_output=>display_data(
      EXPORTING
        value = cdata
    ).

  CATCH cx_root INTO DATA(lx_excpt).
    DATA(lv_msg) = lx_excpt->get_text( ).
ENDTRY.
