*&---------------------------------------------------------------------*
*& Report ZZDD_DEMO_HTTP_CLIENT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzdd_http_test01.

DATA: lo_http_client TYPE REF TO if_http_client,
      lo_rest_client TYPE REF TO cl_rest_http_client,
      lo_request     TYPE REF TO if_rest_entity,
      lo_response    TYPE REF TO if_rest_entity,
      lv_url         TYPE        string,
      lv_body        TYPE        string.

DATA: lv_vendor   TYPE lifnr,
      rv_bpnumber TYPE bp_number.

TRY .


    cl_http_client=>create_by_destination(
         EXPORTING
*       destination              = 'SRMTOSAPCPI'    " Logical destination (specified in function call)
           destination              = 'ID8'
         IMPORTING
           client                   = lo_http_client    " HTTP Client Abstraction
        EXCEPTIONS
            argument_not_found       = 1
            destination_not_found    = 2
            destination_no_authority = 3
            plugin_not_active        = 4
            internal_error           = 5
            OTHERS                   = 6   ).


* Create REST client instance
    CREATE OBJECT lo_rest_client EXPORTING io_http_client = lo_http_client.

* Create request instance
    lo_request = lo_rest_client->if_rest_client~create_request_entity( ).

* Set HTTP version
    lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).

* Set URL here if using the destination
    IF lo_http_client IS BOUND AND lo_rest_client IS BOUND.

* Set the URI if any
      cl_http_utility=>set_request_uri(
        EXPORTING
          request = lo_http_client->request    " HTTP Framework (iHTTP) HTTP Request
          uri     = lv_url      ).             " URI String (in the Form of /path?query-string)

    ENDIF.

    lo_request->set_content_type( EXPORTING iv_media_type = if_rest_media_type=>gc_appl_json ).

*    this CONCATENATE string depends on request body of the service.
* first you shud run it in gw client or any test tool and get the structure of the request body in
*json and accordingly formulate it. IF your request body is too big for a
*CONCATENATE operation then im sorry i don’t know how to form it

    CONCATENATE '{ "number":"' lv_vendor '", "source":"ECC" }' INTO lv_body.
    lo_request->set_string_data( lv_body ).

* HTTP Post
    lo_rest_client->if_rest_resource~post( lo_request ).

* HTTP response
    lo_response = lo_rest_client->if_rest_client~get_response_entity( ).
    lo_response->set_content_type( EXPORTING iv_media_type = if_rest_media_type=>gc_appl_json ).

* HTTP return status
    DATA(http_status)   = lo_response->get_header_field( '~status_code' ).

* HTTP JSON return string
    DATA(json_response) = lo_response->get_string_data( ).




*  convert json to table, navigate
*  the response xml. this is code is very much specific to response body.

    IF json_response IS NOT INITIAL.

      DATA lv_bpnumber TYPE char10.
      DATA(lv_str_data) = /ui2/cl_json=>generate( json = json_response ).
      ASSIGN lv_str_data->* TO FIELD-SYMBOL(<fs_str_data>).
      IF <fs_str_data> IS ASSIGNED.

        ASSIGN COMPONENT 'DATA' OF STRUCTURE <fs_str_data> TO FIELD-SYMBOL(<fs_data>).
        IF <fs_data> IS BOUND.
          ASSIGN <fs_data>->* TO FIELD-SYMBOL(<ft_data>).
          IF <ft_data> IS ASSIGNED.
            ASSIGN COMPONENT 'businessPartnerCode' OF STRUCTURE <ft_data> TO FIELD-SYMBOL(<fs_bpno2>).
            IF <fs_bpno2> IS ASSIGNED.

              ASSIGN <fs_bpno2>->* TO FIELD-SYMBOL(<fs_bpno3>).

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF <fs_bpno3> IS ASSIGNED.
*    rv_bpnumber = <fs_bpno3>.
      ENDIF.
    ENDIF.

  CATCH cx_root INTO DATA(lx_excpt).
    DATA(lv_msg) = lx_excpt->get_text( ).
ENDTRY.
