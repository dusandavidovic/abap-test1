FUNCTION ZZDD_CFG_BAS_DB.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_UPD_IND) TYPE  /RIVER/UPD_IND
*"     VALUE(IT_DATA) TYPE  ZZD_CFG_BAS_T
*"----------------------------------------------------------------------
  IF lines( it_data ) > 0.
    CASE iv_upd_ind.
      WHEN /river/if_c=>db-action-insert
        OR /river/if_c=>db-action-update.
        /river/cl_fugr_svc=>set_table_admin_info( CHANGING ct_table = it_data ).
        MODIFY zzdd_cfg_bas FROM TABLE it_data.
      WHEN /river/if_c=>db-action-delete.
        DELETE zzdd_cfg_bas FROM TABLE it_data.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
ENDFUNCTION.
