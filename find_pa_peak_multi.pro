;---------------------------------------------------------------------------
;Purpose: Find the pitch angle peak according to the def_pap
;Inputs: pa_counts_name, pa_name, pap_name,beta_name
;Keywords:pa_count_line, flux_threshold
;
;Created by Jing Liao
;Created on 06/26/2023
;---------------------------------------------------------------------------

PRO find_pa_peak_multi, pa_counts_name, pa_name, pap_name, region_name, pa_count_line = pa_count_line,flux_threshold=flux_threshold, peak_pa_range = peak_pa_range, def_pap_factor = def_pap_factor, remove_bidirectional_pa = remove_bidirectional_pa

 COMMON SHARE1,ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE
 pa_pa_defined = PA_BINS
;-- Check keywords --
  IF ~KEYWORD_SET(flux_threshold) or n_elements(flux_threshold) ne 3 THEN flux_threshold = [0,0,0] ;[0.1,0.15,0.2]
  IF ~KEYWORD_SET(pa_count_line) THEN pa_count_line = 0
  IF ~KEYWORD_SET(def_pap_factor) OR N_ELEMENTS(def_pap_factor) NE 3 THEN def_pap_factor = [1, 1, 1] ;[1.7,1.4,1.1],;[1.1,1.4,1.7] ;[3,2,1.1]

;-- Load data --
  counts_pa = r_data(pa_counts_name,/Y)
  
  get_data, pa_name, data = data, dlim = dlim, lim = lim
  time_pa = data.x
  flux_pa = data.y           
  pa_pa = data.v         
  
  n_time = N_ELEMENTS(time_pa)
  n_valid_pa = MAX(total(FINITE(flux_pa), 2),/NAN)
  npa = N_ELEMENTS(pa_pa[0, *,0])
  nenergybins = N_ELEMENTS(pa_pa[0,0,*])
;-- set up definition of pitch angle for different magnetosphere regions, using plasma beta --
  def_pap = DBLARR(n_time, nenergybins)
  def_pap_relative = DBLARR(n_time, nenergybins)

  region = r_data(region_name,/Y) MOD 10.
  
  FOR i = 0, n_time-1 DO BEGIN
     for k = 0, nenergybins - 1 do begin
         if region[i] gt 0 then def_pap[i,k] = total(flux_pa[i, *,k],/nan)*def_pap_factor[region[i]-1]/n_valid_pa > flux_threshold[region[i]-1]
         def_pap_relative[i,k] = 1
     endfor 
  ENDFOR   

; pitch angle peak have to be
; 1. local peak for unit flux
; 2. counts greater than a counts threshold  set before
  flux_peak_pa = DBLARR(n_time, npa, nenergybins)
  flux_peak_pa[*, *, *] = !VALUES.F_NAN

  FOR i = 0, n_time-1 DO BEGIN
     FOR k = 0, nenergybins - 1 DO BEGIN 
        FOR j = 0, npa-1 DO BEGIN
            if keyword_set(remove_bidirectional_pa) then remove_bidirectional_pa_cut =  max(flux_pa[i,(npa-j-2 > 0):(npa-j < (npa-1)) ,k] *2  ) > 0 else remove_bidirectional_pa_cut = 0
           IF flux_pa[i, j,k] GE flux_pa[i, j-1 > 0,k]*  def_pap_relative[i,k] AND $
              flux_pa[i, j,k] GE flux_pa[i, j+1 < (npa-1),k]* def_pap_relative[i,k] AND $
              flux_pa[i, j,k] GT def_pap[i,k] AND $
              flux_pa[i, j,k] GT remove_bidirectional_pa_cut AND $
              counts_pa[i, j,k] GT pa_count_line $
           THEN BEGIN
              flux_peak_pa[i, j,k] = flux_pa[i, j,k]
           ENDIF 
        ENDFOR
     ENDFOR 
  ENDFOR

; filter bi-directional pitch angle distribution
;  flux_peak_pa_old =  flux_peak_pa
;  if keyword_set(remove_bidirectional_pa) then begin 
;     FOR i = 0, n_time-1 DO BEGIN
;        FOR k = 0, nenergybins - 1 DO BEGIN
;           this_flux_peak_pa = transpose(flux_peak_pa(i, *, k))
;           valid_pitch_angle_peak = (FINITE(this_flux_peak_pa) - FINITE(REVERSE(this_flux_peak_pa))) eq 1
;           valid_pitch_angle_peak_r = (FINITE(this_flux_peak_pa(1:(npa-1))) - FINITE((REVERSE(this_flux_peak_pa))[0:(npa-2)])) eq 1
;           valid_pitch_angle_peak_l = (FINITE(this_flux_peak_pa(0:(npa-2))) - FINITE((REVERSE(this_flux_peak_pa))[1:(npa-1)])) eq 1
          
;           index = WHERE(valid_pitch_angle_peak EQ 0 OR [valid_pitch_angle_peak[0] ,valid_pitch_angle_peak_r] EQ 0 OR [valid_pitch_angle_peak_l,valid_pitch_angle_peak[npa-1]] EQ 0, ct) 
;           IF ct GT 0 THEN flux_peak_pa(i,index,k) = !VALUES.F_NAN
;        ENDFOR 
;     ENDFOR
;  endif
;stop

; limit the pa peak range as input if keyword is set
  IF KEYWORD_SET(peak_pa_range) THEN BEGIN
     index = where(pa_pa_defined LT min(peak_pa_range) OR pa_pa_defined GT max(peak_pa_range), ct)
     IF ct GT 0 THEN flux_peak_pa[*, index, *] = !VALUES.F_NAN
  ENDIF
  
  str = {x:time_pa, y:flux_peak_pa, v:pa_pa}
  
  store_data, pap_name, data = str, dlim = dlim, lim = lim
  options, pap_name, 'ytitle', 'PAP'
  zlim, pap_name, 0.1, 100

;  stop

;for k = 0, nenergybins-1 do store_data, pap_name+'_'+string(k,format='(i2.2)'), data = {x:time_pa, y:reform(flux_peak_pa[*,*,k]), v:reform(pa_pa[*,*,k])} , dlim =dlim , lim = lim 
;for k = 0, nenergybins -1 do options, pap_name+'_'+string(k,format='(i2.2)'), 'ytitle', 'pa!C'+string(energy_bins[k])

;  stop
END
