; ----------------------------------------------------------------------
; Purpose: find the energy peak and energy range from energy spectrum
; Input: enspec_name : energy spectrum name
; Output: epcut_name:stored energy peak data with the original name + '_epcut'
; erange_name: stored energy range data with the original name +'_erange'
; array all_energy with all the energy bins
;
; by Jing Liao 11/01/2007
; ----------------------------------------------------------------------
pro find_energy_range_from_enspec_multi, enspec_name, epcut_name, erange_name, min_erange_bins = min_erange_bins, round_energybins = round_energybins, energy_peak_limit = energy_peak_limit
  common SHARE1, ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE

  if ~keyword_set(energy_peak_limit) then energy_peak_limit = [min(ENERGY_BINS), max(ENERGY_BINS)]

  energybins = ENERGY_BINS

  ; minimum energy range in energy bins
  if ~keyword_set(min_erange_bins) then min_erange_bins = 1 ; for codif, it was round(n_energybins/16.)

  ; get data from energy spectrum
  get_data, enspec_name, data = data, dlim = dlim, lim = lim
  time = data.x
  flux = data.y
  energy = data.v

  n_energybins = n_elements(flux[0, *])
  n_cut = n_elements(flux[*, 0])

  ; Add a check point now to identify any energy spectra with energy bins different from 16 (cluser/codif), 16(mms/HPCA) or 72 (rbsp/hope)
  if keyword_set(n_energybins) then if n_energybins ne n_elements(ENERGY_BINS) then stop
  
  ; The energy bins in the energy spectrum are not always exactly the same at digits levle (for MMS / HPCA). so we round the enregybins here for later comparisons.
  if keyword_set(round_energybins) then begin
    index_valid = where(finite(energy[*, 0]), ct)
    if ct gt 0 then energybins = round(reform(energy[index_valid[0], *])) else stop
  endif
  ; codif: make sure energy is higer than 35eV to avoid the bad energy bin
  n_energybins_good = n_energybins

  energy_peak = fltarr(n_cut, n_energybins_good)
  energy_range = fltarr(n_cut, n_energybins_good * 2)

  energy_peak[*] = !values.f_nan
  energy_range[*] = !values.f_nan

  ; -----------------------------------------------------------------------
  ; Capture the energy peak as the energy bin wih the maximum flux. Define energy range as the near energy bin with flux less than 0.1 of the maximum flux
  ; -----------------------------------------------------------------------
  for iii = 0, n_cut - 1 do begin
    index_energy_peak = local_max(flux[iii, *])
    if index_energy_peak ne !null then begin
      if keyword_set(energy_peak_limit) then index_energy_peak_limited = where(energy[iii, index_energy_peak] ge min(energy_peak_limit) and energy[iii, index_energy_peak] le max(energy_peak_limit), ct) else ct = 0

      if ct gt 0 then index_energy_peak = index_energy_peak[index_energy_peak_limited]

      energy_peak[iii, index_energy_peak] = energy[iii, index_energy_peak]
      energy_range[iii, index_energy_peak] = energy[iii, (index_energy_peak - 1) > 0]
      energy_range[iii, index_energy_peak + n_energybins_good] = energy[iii, (index_energy_peak + 1) < (n_energybins_good - 1)]

      for jjj = 0, n_elements(index_energy_peak) - 1 do begin
        i_f = (index_energy_peak[jjj] - min_erange_bins) > 0
        while i_f gt 0 do begin
          if flux[iii, index_energy_peak[jjj]] / flux[iii, i_f] le 10. and flux[iii, i_f] le flux[iii, i_f + 1] then begin
            energy_range[iii, index_energy_peak[jjj]] = energy[iii, i_f]
            i_f = i_f - 1
          endif else begin
            i_f = -1
          endelse
        endwhile

        i_f = (index_energy_peak[jjj] + min_erange_bins) < (n_energybins_good - 1)
        while i_f lt n_energybins_good - 1 do begin
          if flux[iii, index_energy_peak[jjj]] / flux[iii, i_f] le 10. and flux[iii, i_f] le flux[iii, i_f - 1] then begin
            energy_range[iii, index_energy_peak[jjj] + n_energybins_good] = energy[iii, i_f]
            i_f = i_f + 1
          endif else begin
            i_f = 100
          endelse
        endwhile
      endfor
    endif
  endfor

  str = {x: time, y: energy_peak, energybins: energybins}
  store_data, epcut_name, data = str, dlim = {psym: -3}

  str = {x: time, y: energy_range, energybins: energybins}
  store_data, erange_name, data = str, dlim = {psym: -3}
end