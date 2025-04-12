; ---------------------------------------------------------------------------
; Purpose: Find the pitch angle peak according to the def_pap
; Inputs: pa_counts_name, pa_name, pap_name,beta_name
; Keywords:pa_count_line, flux_threshold
;
; Created by Jing Liao
; Created on 06/26/2023
; ---------------------------------------------------------------------------

PRO find_pa_peak_multi, pa_counts_name, pa_name, pap_name, region_name, pa_count_line = pa_count_line,flux_threshold=flux_threshold, peak_pa_range = peak_pa_range, def_pap_factor = def_pap_factor, remove_bidirectional_pa = remove_bidirectional_pa, remove_isotropic = remove_isotropic, isotropic_pa_def = isotropic_pa_def

  common SHARE1, ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE
  pa_pa_defined = PA_BINS
  ; -- Check keywords --
  if ~keyword_set(flux_threshold) or n_elements(flux_threshold) ne 3 then flux_threshold = [0, 0, 0] ; [0.1,0.15,0.2]
  if ~keyword_set(pa_count_line) then pa_count_line = 0
  if ~keyword_set(def_pap_factor) or n_elements(def_pap_factor) ne 3 then def_pap_factor = [1, 1, 1] ; [1.7,1.4,1.1],;[1.1,1.4,1.7] ;[3,2,1.1]
  if ~keyword_set(isotropic_pa_def) then isotropic_pa_def = [60,120]

  ; -- Load data --
  counts_pa = r_data(pa_counts_name, /y)

  get_data, pa_name, data = data, dlim = dlim, lim = lim
  time_avg = data.x
  flux_pa = data.y
  pa_pa = data.v

  n_time = n_elements(time_avg)
  n_valid_pa = max(total(finite(flux_pa), 2), /nan)
  npa = n_elements(pa_pa[0, *, 0])
  nenergybins = n_elements(pa_pa[0, 0, *])
; -- set up definition of pitch angle for different magnetosphere regions, using plasma beta --
  def_pap = dblarr(n_time, nenergybins)
  def_pap_relative = dblarr(n_time, nenergybins)

  region = r_data(region_name, /y) mod 10.

  for i = 0, n_time - 1 do begin
    for k = 0, nenergybins - 1 do begin
      if region[i] gt 0 then def_pap[i, k] = total(flux_pa[i, *, k], /nan) * def_pap_factor[region[i] - 1] / n_valid_pa > flux_threshold[region[i] - 1]
      def_pap_relative[i, k] = 1
    endfor
  endfor

  ; pitch angle peak have to be
  ; 1. local peak for unit flux
  ; 2. counts greater than a counts threshold  set before
  flux_peak_pa = dblarr(n_time, npa, nenergybins)
  flux_peak_pa[*, *, *] = !values.f_nan

  for i = 0, n_time - 1 do begin
    for k = 0, nenergybins - 1 do begin
      for j = 0, npa - 1 do begin
        if keyword_set(remove_bidirectional_pa) then remove_bidirectional_pa_cut = max(flux_pa[i, (npa - j - 2 > 0) : (npa - j < (npa - 1)), k] * 2) > 0 else remove_bidirectional_pa_cut = 0
        if flux_pa[i, j, k] ge (flux_pa[i, j - 1 > 0, k] > 0) * def_pap_relative[i, k] and $
          flux_pa[i, j, k] ge (flux_pa[i, j + 1 < (npa - 1), k] > 0) * def_pap_relative[i, k] and $
          flux_pa[i, j, k] gt def_pap[i, k] and $
          flux_pa[i, j, k] gt remove_bidirectional_pa_cut and $
          counts_pa[i, j, k] gt pa_count_line $
        then begin
          flux_peak_pa[i, j, k] = flux_pa[i, j, k]
        endif
      endfor
    endfor
  endfor
  
  ; limit the pa peak range as input if keyword is set
  IF KEYWORD_SET(peak_pa_range) THEN BEGIN
    index = where(pa_pa_defined LT min(peak_pa_range) OR pa_pa_defined GT max(peak_pa_range), ct)
    IF ct GT 0 THEN flux_peak_pa[*, index, *] = !VALUES.F_NAN
  ENDIF

  ; limit the pa peak range as input if keyword is set
  
  if KEYWORD_SET(remove_isotropic) then begin 
    index = where((pa_pa_defined lt max(isotropic_pa_def[*,0]) and pa_pa_defined gt min(isotropic_pa_def[*,0])), ct)
    if ct gt 0 then flux_peak_pa[*, index, *] = !values.f_nan
  endif

  str = {x: time_avg, y: flux_peak_pa, v: pa_pa}

  store_data, pap_name, data = str, dlim = dlim, lim = lim
  options, pap_name, 'ytitle', 'PAP'
  zlim, pap_name, 1, 1000   
  
  ; for k = 0, nenergybins-1 do store_data, pap_name+'_'+string(k,format='(i2.2)'), data = {x:time_avg, y:reform(flux_peak_pa[*,*,k]), v:reform(pa_pa[*,*,k])} , dlim =dlim , lim = lim
  ; for k = 0, nenergybins -1 do options, pap_name+'_'+string(k,format='(i2.2)'), 'ytitle', 'pa!C'+string(energy_bins[k])

  ; time= time_string(time_avg[22:28])
  ; flux = flux_pa[22:28,*,17]
  ; pa = pa_pa[0,*,17]

  ; plot, pa, flux[0,*], /nodata, yrange=[0,500]
  ; for i=0,6 do oplot, pa, flux[i,*], col=i+1 
  ; for i = 0, 6 do xyouts, 50,480-12*i, time[i],col=i+1

  ; i=25
  ; j=3
  ; k=17

  ; stop
  
end