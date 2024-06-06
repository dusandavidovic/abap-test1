FUNCTION zzdd_btp_user_db.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_UPD_IND) TYPE  /RIVER/UPD_IND
*"     VALUE(IT_DATA) TYPE  ZZDD_BTP_USER_T
*"----------------------------------------------------------------------
  IF lines( it_data ) > 0.
    CASE iv_upd_ind.
      WHEN /river/if_c=>db-action-insert
        OR /river/if_c=>db-action-update.
        MODIFY zzdd_btp_user FROM TABLE it_data.
      WHEN /river/if_c=>db-action-delete.
        DELETE zzdd_btp_user FROM TABLE it_data.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
ENDFUNCTION.
