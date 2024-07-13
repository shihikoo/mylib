FUNCTION calculate_B_model, datetime_str, xgsm, ygsm, zgsm, model = model, kp = kp, pdyn = pdyn, Dst = Dst, imfBy = imfBy, imfBz = imfBz

  IF ~KEYWORD_SET(model) THEN model = 't89'
  
  year = STRMID(datetime_str,0,4)
  month =  STRMID(datetime_str,5,2)
  date =  STRMID(datetime_str,8,2)
  hour =  STRMID(datetime_str,11,2)
  minute =  STRMID(datetime_str,14,2)
  second =  STRMID(datetime_str,17,2)
  
  geopack_recalc, year, month, date, hour, minute, second, tilt = tilt, /date

  geopack_igrf_gsm, xgsm, ygsm, zgsm, bx0, by0, bz0

  IF model EQ 't89' THEN BEGIN
; t89 is quiet model and parmod needs to be [1,7], we use 7 for all kp
; > 6
     parmod = (kp + 1) < 7.
     geopack_t89, parmod, xgsm, ygsm, zgsm, dbx, dby, dbz, tilt = tilt 
  ENDIF 

  IF model EQ 't96' THEN BEGIN 
     parmod = [pdyn, Dst, imfBy, imfBz, 0.,0., tilt, xgsm, ygsm, zgsm]
     geopack_t96, parmod, xgsm, ygsm ,zgsm, dbx,dby,dbz,tilt = tilt 
  ENDIF 

  IF model EQ 't01' THEN BEGIN 
     parmod = [pdyn, Dst, imfBy, imfBz, 0.,0., tilt, xgsm, ygsm, zgsm]
     geopack_t01, parmod, xgsm, ygsm ,zgsm, dbx,dby,dbz,tilt = tilt 
  ENDIF 

  IF model EQ 'ts04' THEN BEGIN 
     parmod = [pdyn, Dst, imfBy, imfBz, 0.,0., tilt, xgsm, ygsm, zgsm]
     geopack_ts04, parmod, xgsm, ygsm,zgsm, dbx,dby,dbz,tilt = tilt 
  ENDIF 
  
  b = [bx0,by0,bz0]+[dbx,dby,dbz]
;  b = [dbx,dby,dbz]
;  bx =  20.772242155305857
;  by =  -0.64656309167480264
;  bz = -15.071999589562287
  RETURN, b
END
