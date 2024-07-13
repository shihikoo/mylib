;-----------------------------------------------------------------------
; Purpose: find_pa_range_multi
; Description: To find the pitch angel range of the streaming O+
;
; Inputs: pa_name: tplot name for pitch angle
;         pap_beam_name: identified and filtered pitch angle beam
;         tplot name
; Output: pap_range_name: tplot name for storing pitch angle range of
; the streaming O+
;         pap_int_flux_name: tplot name for storing differential flux
;         value intergrated over pitch angle range. 
;
; Created by Jing Liao
; Created on 01/22/2023
;-----------------------------------------------------------------------


;-------------------------------------------------------
; function to find range around the peak for an array
;-------------------------------------------------------
pro find_range_around_peak, arr, peak_ind, pa, range, range_ind = range_ind, int_arr = int_arr, range_threshold = range_threshold

  COMMON SHARE1,ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE

  if ~KEYWORD_SET(range_threshold) then range_threshold = 1.6487212707

  PI = 3.1415926
  dpa = (ABS(pa[1] - pa[0]))

  phi = 2*PI  ; solid angle with pitch angle converted to rad unit and in gyro direction (2* PI)
  
  narr = n_elements(arr)
  range_ind = INTARR(2)
  range_pa = DBLARR(2)
  
  i_f = peak_ind - 1 > 0
  range_ind[0] = peak_ind
  range_pa[0] = pa[peak_ind]
  last_flux = arr[peak_ind]
  WHILE i_f GT 0 DO BEGIN
     IF arr[peak_ind]/arr[i_f] LE range_threshold and arr[i_f] lt last_flux  THEN BEGIN
        range_ind[0] = i_f
        range_pa[0] = pa[i_f]
        last_flux = arr[i_f]
        i_f = i_f - 1
     ENDIF  ELSE BEGIN  
        i_f = -1
     ENDELSE 
  ENDWHILE  
  
  i_f = peak_ind + 1 < (narr-1)
  range_ind[1] = peak_ind
  range_pa[1] = pa[peak_ind]
  last_flux = arr[peak_ind]
  WHILE i_f LT narr  DO BEGIN
     IF arr[peak_ind]/arr[i_f] LE range_threshold  and arr[i_f] lt last_flux  THEN BEGIN
        range_ind[1] = i_f
        range_pa[1] = pa[i_f]
        last_flux = arr[i_f]
        i_f = i_f + 1
     ENDIF ELSE BEGIN 
        i_f = 100
     ENDELSE 
  ENDWHILE 

  int_arr = 0
;  for i = 0,n_elements(range_ind)-1 do begin
  for i = 0, n_elements(pa)-1 do begin
     index = i
  ;index = range_ind[i]
     int_arr = int_arr + phi * arr[index] * ((cos(PI/180*(pa[index]-dpa/2)))^2 - (cos(PI/180*(pa[index]+dpa/2)))^2)/2
;    print,index
;    print,int_arr
  endfor 
  
;  int_arr = TOTAL(arr[range_ind[0]:range_ind[1]] * dpa)
  range = ABS(range_pa[1] - range_pa[0])
;  stop
end 


;-------------------------
; main function
;--------------------------
pro find_pa_range_multi,  pa_name, pap_beam_name, en_name, pap_range_name, int_flux_name
;   get_data, en_name, data = data
;   en_en = data.v
  
  get_data, pa_name, data = data, dlim = dlim_pa,lim=lim_pa
  time_pa = data.x
  flux_pa = data.y
  pa_pa = data.v
  
  get_data, pap_beam_name, data = data
  flux_beam = data.y

  n_time = N_ELEMENTS(time_pa)
  n_pa = N_ELEMENTS(pa_pa[0, *,0])
  n_energybins = N_ELEMENTS(pa_pa[0,0,*])

  pa_range = DBLARR(n_time, n_pa, n_energybins)
  pa_range[*,*,*] = !VALUES.F_NAN
  int_flux = DBLARR(n_time, n_pa, n_energybins)
  int_flux[*,*,*] = !VALUES.F_NAN

  FOR i = 0, n_time-1 DO BEGIN
     for k = 0, n_energybins - 1 do begin
        for j = 0, n_pa - 1 do begin  
           if flux_beam[i,j,k] gt 0 then begin
              find_range_around_peak, flux_pa[i,*,k], j, pa_pa[i,*,k], range, int_arr = int_arr
              pa_range[i,j,k] = range
              int_flux[i,j,k] = int_arr
           endif 
        endfor 
     endfor 
  ENDFOR  
 
  str = {x:time_pa, y:pa_range, v:pa_pa}
  store_data, pap_range_name, data = str, dlim = dlim_pa, lim = lim_pa
  options, pap_range_name, 'ytitle', 'PA range'
  options, int_flux_name, 'ztitle', 'degree'
  
  str = {x:time_pa, y:int_flux, v:pa_pa}  
  store_data, int_flux_name, data = str, dlim = dlim_pa, lim = lim_pa
  options, int_flux_name, 'ytitle', 'int flux'
  options, int_flux_name, 'ztitle', '(cm!U2!N s eV)!U-1'
;  stop
end
