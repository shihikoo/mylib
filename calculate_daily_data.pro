FUNCTION calculate_daily_data, data

  ; Earth_radius = 6371.                                                        ;km
  ; mass_o = 16*1.6e-27*(1e3)^2/(1.6e-19)                                       ; unit: ev/(km/s)^2
  ; mag_normal = 31200.*(6370./(6370+1000))^3*sqrt(1+3*sin(80*3.1415926/180)^2) ; =39832.1 ; dipole field at 1000km and 80 invariant latitude ;33695.9 ;nT at 60 invariant latitude degree from Seki 1998
  ; norm_factor_mlt = 1/24.*360/180*!PI
  
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
  
  data = CREATE_STRUCT(data, 'b_model', b)        
  data = CREATE_STRUCT(data, 'flLen_para', flLen_para) 
  data = CREATE_STRUCT(data, 'flLen_anti', flLen_anti) 

;  energy_v_tail = sqrt(2*energy_tail/mass_o)
;  energy_v_earth = sqrt(2*energy_earth/mass_o)
;  v_energy_tail = mass_o*velocity_tail^2/2
;  v_energy_earth = mass_o*velocity_earth^2/2
;  mag = sqrt(data.Bx_GSM^2+data.By_GSM+data.Bz_GSM^2)
;  imf_b = sqrt(data(*, 43)^2+data(*, 44)^2+data(*, 45)^2)
;  proton_v=sqrt(proton_vx^2+proton_vy^2+proton_vz^2)                 
;  proton_T=sqrt(proton_Tx+proton_Ty+proton_Tz)                                
;  VxBz=sw_v*(imf_bz<0)*(1e3*1e-9*1e3) ;calcualted SW Ey. sw velocity are considered to be as sw Vx here and only keep southward Bz since dayside reconnection brings the Ey in. unit is mV/m 
;  Vx_tail(index)=0 & Vy_tail(index)=0 & Vz_tail(index)=0            
;  Vx_earth(index)=0 & Vy_earth(index)=0 & Vz_earth(index)=0             
; V_paraE_tail = (Vx_tail*Ex+Vy_tail*Ey+Vz_tail*Ez)/sqrt(Ex^2+Ey^2+Ez^2)   
; V_paraE_earth = (Vx_earth*Ex+Vy_earth*Ey+Vz_earth*Ez)/sqrt(Ex^2+Ey^2+Ez^2)   
; V_paraE_proton = (proton_Vx*Ex+proton_Vy*Ey+proton_Vz*Ez)/sqrt(Ex^2+Ey^2+Ez^2)
  
  RETURN, data

END
