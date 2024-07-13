;---------------------------------------------------------------
; Purpose: caluclate denergy with given energy range. The output
; denergy is FWHM. energy +/- denergy is the full expression
; -------------------------------------------------------------
FUNCTION calculate_denergy_from_energy_fwhm, energy, epcut

  A = (energy - SHIFT(energy,[0,1])) / (energy + SHIFT(energy,[0,1]))
  A[*,0] = A[*,1]
  
  energy_low  = energy * (1. - A)
  energy_high = energy * (1. + A)

  all_denergy = (energy_high - energy_low)/2
  
  n_time = N_ELEMENTS(epcut)
  epcut_denergy = DBLARR(n_time) 
  epcut_denergy[*] = !VALUES.F_NAN
  index = WHERE(FINITE(epcut), ct)
  IF ct GT 0 THEN BEGIN   
     FOR iepcut = 0, N_ELEMENTS(index)-1 DO BEGIN
        ind_energy_bin = WHERE(energy[index[iepcut],*] EQ epcut[index[iepcut]] )
        epcut_denergy[index[iepcut]] = all_denergy[index[iepcut], ind_energy_bin]        
     ENDFOR
  ENDIF 
  
  RETURN, epcut_denergy
END
