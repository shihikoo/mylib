;----------------------------------------------------------------
; Purpose:  Convert pitch angle plot to earth tail direction and then
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

PRO filter_beams, pap_name, epcut_name, erange_name, pap_beam_name, epcut_beam_name, erange_beam_name, diff_e = diff_e, diff_pa = diff_pa

;--------------------------------------------------------------------
;load input data and basic settings
;--------------------------------------------------------------------
  get_data, pap_name, data = data, dlim = dlim, lim = lim
  time_avg= data.x
  flux_pap = data.y
  pa_pap = data.v

  get_data, epcut_name, data = data
  energy_peak = data.y
  nenergy = N_ELEMENTS(data.energybins)
  energybins = data.energybins

  get_data, erange_name, data = data
  energy_range = data.y
  
  n_time = N_ELEMENTS(time_avg)
  npa = N_ELEMENTS(pa_pap(0, *))
  IF ~KEYWORD_SET(diff_e) THEN diff_e = 2  ; accepted energy bin difference 
  IF ~KEYWORD_SET(diff_pa) THEN diff_pa = 2 ; accepted pitch bin difference
  IF ~KEYWORD_SET(diff_time) THEN diff_time = 1 ; beams are defined as beam before and after 1 times of average time. 

;-----------------------------------------------------------------------
;check for the beam. It requires beam to be equal or more than 3 time
;of the average time.
;-----------------------------------------------------------------------
;  flux_beam = flux_pap_et
  flux_beam = flux_pap
  epcut_beam = energy_peak
  erange_beam = energy_range

;check energy (longer than 2 times of average_time)
  index_ep = INTARR(n_time)
  FOR k = 0, n_time-1 DO  index_ep(k) = where(round(energybins) EQ round(epcut_beam(k))) 
  ind=where(index_ep eq -1, ct)
;if ct gt 0 then index_ep(ind)=-1 ; -100 as a flag for no energy peak or no beam
  ep_beam = index_ep
  ep_beam(*) = !values.f_nan
  IF n_time GT 2 THEN BEGIN
     FOR i = 1, n_time-2 DO BEGIN
        if index_ep(i) ne -1 then begin 
           l=ABS(index_ep(i)-index_ep(i-1)) le diff_e and index_ep(i-1) ne -1
           r=ABS(index_ep(i)-index_ep(i+1)) le diff_e and index_ep(i+1) ne -1
           ep_beam(i)=l+r
        endif else ep_beam(i)=0
     ENDFOR

; add the first time and last time back
     IF ep_beam(1) EQ 2 THEN ep_beam(0) = 1
     IF ep_beam(n_time-2) EQ 2 THEN ep_beam(n_time-1) = 1

; add the beam edge point back
     FOR i = 1, n_time-2 DO BEGIN 
        IF ep_beam(i) EQ 2 THEN BEGIN 
           IF ep_beam(i-1) EQ 1 AND ABS(index_ep(i)-index_ep(i-1)) LE diff_e THEN ep_beam(i-1) = ep_beam(i-1)+1
           IF ep_beam(i+1) EQ 1 AND ABS(index_ep(i)-index_ep(i+1)) LE diff_e THEN ep_beam(i+1) = ep_beam(i+1)+1
        ENDIF
     ENDFOR
  endif  else ep_beam(*)=0

; only the points with both sides having beam remain
  gap = where(ep_beam lt 2, ct)
;put data with no beam into Nan
  IF ct GT 0 THEN BEGIN
     index_ep(gap) = -1
     flux_beam(gap, *) = !VALUES.F_NAN 
  ENDIF  

; Check flow pitch angle. A beam needs to have similar pitch angle. 
; Check the pitch angle of the peak flux flow. If one has to have both
; left and right neighbor within +/-1 pitch angle, then it is
; considered a beam. Adding the edge points of the beam later
; Hence the beam lasts 3 * average_time minimum
  nbeam = intarr(n_time, npa)
; check the left neighbor
  FOR i = 1, n_time-1 DO BEGIN   
     FOR j = 0, npa-1 DO BEGIN 
        IF flux_beam(i, j) GT 0 THEN nbeam(i, j) = TOTAL(flux_beam(i-1,((j-diff_pa) > 0):((j+diff_pa) < (npa-1))) GT 0) GT 0
     ENDFOR 
  ENDFOR
; check the right neighbor
  FOR i = 0, n_time-2 DO BEGIN   
     FOR j = 0, npa-1 DO BEGIN 
        IF flux_beam(i, j) GT 0 THEN nbeam(i, j) = nbeam(i,j) + (TOTAL(flux_beam(i+1, ((j-diff_pa) > 0):((j+diff_pa) < (npa-1))) GT 0) GT 0)
     ENDFOR
  ENDFOR

; add the edge point of a beam back as 2
  FOR i = 1, n_time-2 DO BEGIN 
     FOR j = 0, npa-1 DO BEGIN 
        IF nbeam(i, j) EQ 2 THEN BEGIN
           IF total(nbeam(i-1, (j-diff_pa > 0):(j+diff_pa < (npa-1))), 2) EQ 1  THEN BEGIN 
              nbeam(i-1, (j-diff_pa > 0):(j+diff_pa < (npa-1))) = nbeam(i-1, ((j-diff_pa) > 0):((j+diff_pa) < (npa-1))) + 1
           ENDIF
           IF total(nbeam(i+1, (j-diff_pa > 0):(j+diff_pa < (npa-1))), 2) EQ 1 THEN BEGIN 
              nbeam(i+1, ((j-diff_pa) > 0):((j+diff_pa) < (npa-1))) = nbeam(i+1, ((j-diff_pa) > 0):((j+diff_pa) < (npa-1)))+ 1
           ENDIF
        ENDIF
     ENDFOR
  ENDFOR

; so anything here marked as 0 or 1 is not beam
  ind=where(nbeam le 1, ct) 
  if ct gt 0 then begin 
     nbeam(ind) = 0
     flux_beam(ind) = !VALUES.F_NAN
  endif 

;record all non-beam filtered with direction also into energy peak string
  loc = where(total(nbeam(*, *), 2,/nan) EQ 0, ct)
  IF ct GT 0 THEN BEGIN 
     index_ep(loc) = -1
     epcut_beam(where(index_ep EQ -1)) = !VALUES.F_NAN
     erange_beam(where(index_ep EQ -1),*) = !VALUES.F_NAN
  ENDIF 

;check energy again(longer than 2 times of average_time)
;if ct gt 0 then index_ep(ind)=-1 ; -100 as a flag for no energy peak or no beam
  ep_beam = index_ep
  ind=where(index_ep eq -1,ct) 
  ep_beam(ind) = !values.f_nan
  IF n_time GT 2 THEN BEGIN
     FOR i = 1, n_time-2 DO BEGIN
        if index_ep(i) ne -1 then begin 
           l=ABS(index_ep(i)-index_ep(i-1)) le diff_e and index_ep(i-1) ne -1
           r=ABS(index_ep(i)-index_ep(i+1)) le diff_e and index_ep(i+1) ne -1
           ep_beam(i)=l+r
        endif else ep_beam(i)=0
     ENDFOR
; add the first time and last time back
     IF ep_beam(1) EQ 2 THEN ep_beam(0) = 1
     IF ep_beam(n_time-2) EQ 2 THEN ep_beam(n_time-1) = 1

; add the beam edge point back
     FOR i = 1, n_time-2 DO BEGIN 
        IF ep_beam(i) EQ 2 THEN BEGIN 
           IF ep_beam(i-1) EQ 1 AND ABS(index_ep(i)-index_ep(i-1)) LE diff_e THEN ep_beam(i-1) = ep_beam(i-1)+1
           IF ep_beam(i+1) EQ 1 AND ABS(index_ep(i)-index_ep(i+1)) LE diff_e THEN ep_beam(i+1) = ep_beam(i+1)+1
        ENDIF
     ENDFOR
  endif  else ep_beam(*)=0

; only the points with both sides having beam remain
  gap = where(ep_beam lt 2, ct)
;put data with no beam into Nan
  IF ct GT 0 THEN BEGIN
;     index_ep(gap) = -1
     flux_beam(gap, *) = !VALUES.F_NAN 
     epcut_beam(gap) = !VALUES.F_NAN
     erange_beam(gap,*) = !VALUES.F_NAN
  ENDIF

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

END
