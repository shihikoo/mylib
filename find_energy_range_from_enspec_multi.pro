;----------------------------------------------------------------------
; Purpose: find the energy peak and energy range from energy spectrum
; Input: enspec_name : energy spectrum name
; Output: epcut_name:stored energy peak data with the original name + '_epcut'
;         erange_name: stored energy range data with the original name +'_erange'
;         array all_energy with all the energy bins
;
; by Jing Liao 11/01/2007
;----------------------------------------------------------------------
PRO find_energy_range_from_enspec_multi, enspec_name, epcut_name, erange_name, min_erange_bins = min_erange_bins, round_energybins = round_energybins

  COMMON SHARE1,ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE

  energybins = energy_bins
  
; minimum energy range in energy bins  
  if ~keyword_set(min_erange_bins) then min_erange_bins = 1 ; for codif, it was round(n_energybins/16.)

; get data from energy spectrum
  get_data, enspec_name, data = data, dlim = dlim, lim = lim
  time = data.x & flux = data.y & energy = data.v
  
  n_energybins = N_ELEMENTS(flux[0, *])
  n_cut = N_ELEMENTS(flux[*, 0])

; Add a check point now to identify any energy spectra with energy bins different from 16 (cluser/codif), 16(mms/HPCA) or 72 (rbsp/hope)
  if keyword_set(nenergybins) then begin
     IF n_energybins NE  N_ELEMENTS(RBSP_ENERGY_BINS)  THEN stop
  endif 
; The energy bins in the energy spectrum are not always exactly the
; same at digits levle (for MMS / HPCA). so we round the enregybins here for later comparisons.
  if keyword_set(round_energybins) then begin 
     index_valid = WHERE(FINITE(energy[*,0]), ct)
     IF ct GT 0 THEN energybins = ROUND(reform(energy[index_valid[0], *])) ELSE stop
  endif 
; codif: make sure energy is higer than 35eV to avoid the bad energy bin
  n_energybins_good = n_energybins
  
  energy_peak = FLTARR(n_cut, n_energybins_good)
  energy_range = FLTARR(n_cut, n_energybins_good*2)

  energy_peak[*] = !VALUES.F_NAN
  energy_range[*] = !VALUES.F_NAN
  
;-----------------------------------------------------------------------
; Capture the energy peak as the energy bin wih the maximum flux. Define energy range as the near energy bin with flux less than 0.1 of the maximum flux
;-----------------------------------------------------------------------
  FOR iii = 0, n_cut - 1 DO BEGIN
     index_energy_peak = local_max(flux[iii,*])
     
     IF index_energy_peak NE !NULL then begin 
        energy_peak[iii, index_energy_peak] = energy[iii, index_energy_peak]
        energy_range[iii, index_energy_peak] = energy[iii, (index_energy_peak-1) > 0]
        energy_range[iii, index_energy_peak + n_energybins_good] =  energy[iii, (index_energy_peak+1) < (n_energybins_good-1)] 

        for jjj = 0, N_ELEMENTS(index_energy_peak) - 1 DO BEGIN  
           i_f = (index_energy_peak[jjj] - min_erange_bins) > 0
           WHILE i_f GT 0 DO BEGIN
              IF flux[iii, index_energy_peak[jjj]]/flux[iii, i_f] LE 10. AND flux[iii, i_f] LE flux[iii, i_f+1]  THEN BEGIN  
                 energy_range[iii, index_energy_peak[jjj]] = energy[iii, i_f]
                 i_f = i_f-1
              ENDIF  ELSE BEGIN  
                 i_f = -1
              ENDELSE  
           ENDWHILE  
           
           i_f = (index_energy_peak[jjj] + min_erange_bins) < (n_energybins_good - 1)
           WHILE i_f LT n_energybins_good-1  DO BEGIN
              IF flux[iii, index_energy_peak[jjj]]/flux[iii, i_f] LE 10. AND flux[iii, i_f] LE flux[iii, i_f-1]  THEN BEGIN  
                 energy_range[iii, index_energy_peak[jjj]+n_energybins_good] = energy[iii, i_f]
                 i_f = i_f+1
              ENDIF ELSE BEGIN 
                 i_f = 100
              ENDELSE 
           ENDWHILE
          
        ENDFOR
        
     ENDIF                  
  ENDFOR

  str = {x: time, y: energy_peak, energybins: energybins}
  store_data, epcut_name, data = str, dlim = {psym: -3}
  
  str = {x: time, y: energy_range, energybins: energybins }
  store_data, erange_name, data = str, dlim = {psym: -3}

END
