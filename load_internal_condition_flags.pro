FUNCTION load_flag, arr, range, set_nan = set_nan

  flag = arr GT range[0] AND arr LT range[1]
  flag = FLOAT(flag)

  IF KEYWORD_SET(set_nan) THEN BEGIN 
     index = WHERE(~FINITE(arr), ct)
     IF ct GT 0 THEN flag[index] = !VALUES.F_NAN

     index = WHERE(flag EQ 0, ct)
     IF ct GT 0 THEN flag[index] = !VALUES.F_NAN
  ENDIF 

  RETURN, flag
END

PRO load_energy_flag, data, energy_filter, energy_flag_para, energy_flag_anti
  energy_flag_para = load_flag(data.en_para, energy_filter)
  energy_flag_anti = load_flag(data.en_anti, energy_filter)
END

; Here we used "_1h" solar wind paramenters to use the solar wind parameters that has been averaged over the previouse 1 hour 
PRO load_imfBz_flag, data, imfBz_filter, imfBz_flag_para, imfBz_flag_anti
  imfBz_flag_para = load_flag(data.IMF_Bz_para_1h, imfBz_filter,/set_nan)
  imfBz_flag_anti = load_flag(data.IMF_Bz_anti_1h, imfBz_filter,/set_nan)
END

PRO load_imfBy_flag, data, imfBy_filter, imfBy_flag_para, imfBy_flag_anti
  imfBy_flag_para = load_flag(data.IMF_By_para_1h, imfBy_filter,/set_nan)
  imfBy_flag_anti = load_flag(data.IMF_By_anti_1h, imfBy_filter,/set_nan)
END

PRO load_swP_flag, data, swP_filter, swP_flag_para, swP_flag_anti  
  swP_flag_para = load_flag(data.sw_P_para_1h, swP_filter,/set_nan)
  swP_flag_anti = load_flag(data.sw_P_anti_1h, swP_filter,/set_nan)
END

PRO load_swV_flag, data, swV_filter, swV_flag_para, swV_flag_anti  
  swV_flag_para = load_flag(abs(data.sw_V_para_1h), swV_filter,/set_nan)
  swV_flag_anti = load_flag(abs(data.sw_V_anti_1h), swV_filter,/set_nan)
END

PRO load_direction_flag, data, direction, direction_flag_para, direction_flag_anti
  ndata = N_ELEMENTS(data.Time)
  direction_flag_para = DBLARR(ndata)
  direction_flag_anti = DBLARR(ndata)

  IF direction EQ 'para' THEN BEGIN 
     direction_flag_para[*] = 1 
     direction_flag_anti[*] = 0
  ENDIF 
  
  IF direction EQ 'both' THEN BEGIN 
     direction_flag_para[*] = data.flag_para EQ 1 AND data.flag_anti EQ 1
     direction_flag_anti[*] = data.flag_para EQ 1 AND data.flag_anti EQ 1
  ENDIF 

  IF direction EQ 'anti' THEN BEGIN 
     direction_flag_para[*] = 0 
     direction_flag_anti[*] = 1
  ENDIF 

  IF direction EQ 'any' THEN BEGIN 
     direction_flag_para[*] = 1
     direction_flag_anti[*] = 1
  ENDIF

  if direction eq 'outflow' then begin
     direction_flag_para[*] = 0
     direction_flag_anti[*] = 0
     
     index = where((data.gsm_x gt -1 and data.gsm_z le 0) or (data.gsm_x le -1 and data.bx_gsm le 0), ct)
     if ct gt 0 then direction_flag_para[index] = 1    

     index = where((data.gsm_x gt -1 and data.gsm_z gt 0) or (data.gsm_x le -1 and data.bx_gsm gt 0), ct)
     if ct gt 0 then direction_flag_anti[index] = 1

  endif

  direction_flag_para = FLOAT(direction_flag_para)
  direction_flag_anti = FLOAT(direction_flag_anti)
 
 END

;----------------------------------------------------------------
; Purpose: load internal condition flags. Internal condition is
; defined as conditions that are different for parallel and
; antiparallel O+ beams. All solar wind conditions are delayed
; according to the O+ beam velocities, and that is the reason the
; solar wind conditions are considered internal conditions here
;----------------------------------------------------------------

PRO load_internal_condition_flags, data, direction, energy_filter, imfBz_filter, imfBy_filter, swP_filter,swV_filter, flag_int_condition_para, flag_int_condition_anti
 
 load_energy_flag, data, energy_filter, energy_flag_para, energy_flag_anti
 load_imfBz_flag, data, imfBz_filter, imfBz_flag_para, imfBz_flag_anti
 load_imfBy_flag, data, imfBy_filter, imfBy_flag_para, imfBy_flag_anti
 load_swP_flag, data, swP_filter, swP_flag_para, swP_flag_anti
 load_swV_flag, data, swV_filter, swV_flag_para, swV_flag_anti
 load_direction_flag, data, direction, direction_flag_para, direction_flag_anti
 
 flag_int_condition_para = direction_flag_para * energy_flag_para * imfBz_flag_para * swP_flag_para * imfBy_flag_para *  swV_flag_para
 flag_int_condition_anti = direction_flag_anti * energy_flag_anti * imfBz_flag_anti * swP_flag_anti * imfBy_flag_anti *  swV_flag_anti

END
