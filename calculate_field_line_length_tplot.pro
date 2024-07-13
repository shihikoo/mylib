FUNCTION calculate_daily_data, dispersion_name, xgsm_name, ygsm_name, zgsm_name,kp_name, swp_name, dst_name, imfby_name,imfbz_name

  get_data, dispersion_name, data = data
  time_dispersion = data.x
  dispersion = data.y
  index = where(FINITE(data.y), ct_dispersion)
  if ct_dispersion eq 0 then RETURN, 0


  xgsm = r_data(xgsm_name,/Y)
  ygsm = r_data(ygsm_name,/Y)
  zgsm = r_data(zgsm_name,/Y)
  kp = r_data(kp_name,/Y)
  pdyn = r_data(swp_name,/Y)
  dst = r_data(dst_name,/Y)
  imfby = r_data(imfby_name,/Y)
  imfbz = r_data(imfbz_name,/Y)

  ndata = N_ELEMENTS(data.Time)
  b = DBLARR(ndata) &   b[*] = !VALUES.F_NAN
  flLen_para = DBLARR(ndata) &   flLen_para[*] = !VALUES.F_NAN
  flLen_anti = DBLARR(ndata) &   flLen_anti[*] = !VALUES.F_NAN

  model = 't89'

  FOR ii = 0, ndata-1 DO BEGIN 
     IF ~FINITE(data.GSM_X[ii]) OR ~FINITE(DATA.kp[ii]) THEN CONTINUE
    ;  print,ii 
    
     b[ii] = SQRT(TOTAL((calculate_b_model(time_string(data.time[ii]), data.GSM_X[ii], data.GSM_Y[ii],data.GSM_Z[ii],kp = data.kp[ii], pdyn = data.SW_P[ii], Dst = data.Dst[ii], imfBy = data.IMF_By[ii], imfBz = data.IMF_Bz[ii], model=model))^2))      

     flLen = calculate_field_line_length(time_string(data.time[ii]), data.GSM_X[ii], data.GSM_Y[ii],data.GSM_Z[ii],kp = data.kp[ii], pdyn = data.SW_P[ii], Dst = data.Dst[ii], imfBy = data.IMF_By[ii], imfBz = data.IMF_Bz[ii], model=model)
     flLen_para[ii] = flLen[0]
     flLen_anti[ii] = flLen[1]
  ENDFOR 

  ; store_data, 
  
  RETURN, 1

END
