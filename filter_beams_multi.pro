;----------------------------------------------------------------
; Purpose:  Continuity check of the beam. 1. Check the energy peak,
; 2. check the pitch angle peak. 3. review the energy peak again.
; filter energy peak and erange data with beam duration (continuity check)
;
; Input: pap_name
;        cpcut_name
;        erange_name
;        bx_name
;        x_gse_name
;        bz_gsm_name
;        pap_beam_et_name
;        epcut_beam_name
;        erange_beam_name
; 
; Created by Jing Liao
; Created on 03/16/2012  
; Modified on 05/01/2021
;--------------------------------------------------------------------

PRO filter_beams_multi, pap_name, epcut_name, erange_name, pap_beam_name, epcut_beam_name, erange_beam_name, diff_en = diff_en, diff_pa = diff_pa

;--------------------------------------------------------------------
;load input data and basic settings
;--------------------------------------------------------------------
  get_data, pap_name, data = data, dlim = dlim, lim = lim
  time_avg= data.x
  flux_pap = data.y
  pa_pap = data.v

  get_data, epcut_name, data = data
  energy_peak = data.y
  nen = N_ELEMENTS(data.energybins)
  energybins = data.energybins

  energy_range = r_data(erange_name,/Y)
  
  n_time = N_ELEMENTS(time_avg)
  npa = N_ELEMENTS(pa_pap[0, *])
  
  IF ~KEYWORD_SET(diff_en) THEN diff_en = 1   ; accepted energy bin difference 
  IF ~KEYWORD_SET(diff_pa) THEN diff_pa = 1 ; accepted pitch bin difference
;  IF ~KEYWORD_SET(diff_time) THEN diff_time = 1 ; beams are defined as beam before and after 1 times of average time. 

; initalize all beam arrays with inital peak arrays
  flux_beam = flux_pap
  epcut_beam = energy_peak
  erange_beam = energy_range
  
;-----------------------------------------------------------------------
; Check the pitch angle peak flux data. A beam needs to have similar
; continuety in both energy and pitch angle. 
; If one has to have both left and right neighbor within defiend
; energy and pitch angle range, then it is considered a beam. 
; Adding the edge points of the beam later
; Hence the beam lasts 3 * average_time minimum
;------------------------------------------------------------------------
  beam_flag = intarr(n_time, npa, nen)

; search through for continuity
  FOR i = 1, n_time-2 DO BEGIN
     FOR j = 0, npa-1 DO BEGIN 
        for k = 0, nen - 1 do begin
           IF flux_beam(i, j,k) GT 0 THEN BEGIN 
              l = TOTAL(flux_beam(i-1, ((j-diff_pa) > 0):((j+diff_pa) < (npa-1)), ((k-diff_en) > 0):((k+diff_en) < (nen-1))) GT 0) GT 0
              r = TOTAL(flux_beam(i+1, ((j-diff_pa) > 0):((j+diff_pa) < (npa-1)), ((k-diff_en) > 0):((k+diff_en) < (nen-1))) GT 0) GT 0
              beam_flag(i, j,k) = l + r
           ENDIF
        ENDFOR
     endfor 
  ENDFOR 

; add the edge point of a beam back as 2
  FOR i = 1, n_time-2 DO BEGIN 
     FOR j = 0, npa-1 DO BEGIN
        for k = 0, nen - 1 do begin
           IF beam_flag(i, j, k) EQ 2 THEN BEGIN
              temp =  beam_flag(i-1, (j-diff_pa > 0):(j+diff_pa < (npa-1)),  ((k-diff_en) > 0):((k+diff_en) < (nen-1)))
              index = WHERE(temp EQ 1, ct)
              IF ct GT 0 THEN BEGIN 
                 temp[index] = temp[index] + 1
                 beam_flag(i-1, (j-diff_pa > 0):(j+diff_pa < (npa-1)),  ((k-diff_en) > 0):((k+diff_en) < (nen-1))) = temp
              ENDIF

              temp =  beam_flag(i+1, (j-diff_pa > 0):(j+diff_pa < (npa-1)),  ((k-diff_en) > 0):((k+diff_en) < (nen-1)))
              index = WHERE(temp EQ 1, ct)
              IF ct GT 0 THEN BEGIN 
                 temp[index] = temp[index] + 1
                 beam_flag(i+1, (j-diff_pa > 0):(j+diff_pa < (npa-1)),  ((k-diff_en) > 0):((k+diff_en) < (nen-1))) = temp
              ENDIF           
           ENDIF
        ENDFOR
     endfor
  ENDFOR

; so anything in pitch angle beam array less  than 2 is not identified as a beam
  index = where(beam_flag LT 2, ct)
; save the filtered results into flux_beam
  if ct gt 0 then flux_beam(index) = !VALUES.F_NAN

;  index = where(TOTAL(TOTAL(beam_flag EQ 2, 2,/nan),2,/nan) EQ 0, ct)
;  IF ct GT 0 THEN  pa_pap[index,*,*] = !VALUES.F_NAN
  
; save the filtered restuls into energy peak and range arrays
  index = where(total(beam_flag EQ 2, 2,/nan) EQ 0, ct)
  if ct gt 0 then BEGIN

     epcut_beam[index] = !VALUES.F_NAN
       
     upper_range = (erange_beam[*,(nen):(2*nen-1)])
     lower_range = (erange_beam[*,0:(nen-1)])
     upper_range[index] =  !VALUES.F_NAN
     lower_range[index] =  !VALUES.F_NAN
     (erange_beam[*,(nen):(2*nen-1)]) = upper_range
     (erange_beam[*,0:(nen-1)]) =  lower_range 
  endif 
;-----------------------------------------------------------------------------
;save into pitch angle
 ;----------------------------------------------------------------------------
  str = {x:time_avg, y:flux_beam, v:pa_pap}
  store_data, pap_beam_name, data = str, dlim=dlim,lim=lim
  options, pap_beam_name, 'ytitle', 'Pitch Angle!C!CBeam'

  str = {x:time_avg, y:epcut_beam, energybins:energybins}
  store_data, epcut_beam_name, data = str, dlim = {psym:-7}

  str = {x:time_avg, y:erange_beam, energybins:energybins}
  store_data, erange_beam_name, data = str, dlim = {psym:-7}

  zlim, "mms1_hpca_oplus_eflux_pa_red_000_060_nflux",0.1,100
  options, "mms1_hpca_oplus_eflux_pa_red_000_060_nflux_epcut_beam" , 'color', 1

;  for k = 0, nen-1 do store_data, pap_beam_name+'_'+string(k,format='(i2.2)'), data = {x:time_avg, y:reform(flux_beam[*,*,k]), v:reform(pa_pap[*,*,k])} , dlim =dlim , lim = lim 
;  for k = 0, nen -1 do options, pap_beam_name+'_'+string(k,format='(i2.2)'), 'ytitle',string(k)

;  popen,'test.ps'
;  tplot,["mms1_hpca_oplus_eflux_pa_red_000_060_nflux",'PAs1_hpca_oplus_eflux_pa_re_nfluxa_red_000_060_nflux_PAP_beam_*' ]
;  tplot_panel, v = "mms1_hpca_oplus_eflux_pa_red_000_060_nflux", o = epcut_beam_name, psym = 1
;  pclose
 ; stop
END
