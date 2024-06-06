*&---------------------------------------------------------------------*
*& Report ZZDD_HTTP_VALIDATE_EMAIL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzdd_http_validate_email.

PARAMETERS: p_mail(100) LOWER CASE.                 " E-Mail id to be verified
DATA: http_client TYPE REF TO if_http_client .
DATA: w_string TYPE string,
      w_result TYPE string,
      r_str    TYPE string.
DATA: result_tab TYPE TABLE OF string.

START-OF-SELECTION .
  CLEAR w_string .
  CONCATENATE 'http://www.webservicex.net/ValidateEmail.asmx/IsValidEmail?Email=' p_mail INTO w_string .
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = w_string
    IMPORTING
      client             = http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.
  CALL METHOD http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2.
  CALL METHOD http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3.
  CLEAR w_result .
  w_result = http_client->response->get_cdata( ).
  REFRESH result_tab .
  SPLIT w_result AT cl_abap_char_utilities=>cr_lf INTO TABLE result_tab .
  READ TABLE result_tab INTO r_str INDEX 2.
  IF r_str+44(1) = 't'.
    WRITE:/ 'Valid email address'.
  ELSE.
    WRITE:/ 'Invalid email address'.
  ENDIF.
