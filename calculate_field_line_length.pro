FUNCTION calculate_field_line_length, datetime_str, xgsm, ygsm, zgsm, model = model, kp = kp, pdyn = pdyn, Dst = Dst, imfBy = imfBy, imfBz = imfBz

  IF ~KEYWORD_SET(model) THEN model = 't89'

  year = STRMID(datetime_str,0,4) &  month = STRMID(datetime_str,5,2) & date = STRMID(datetime_str,8,2)
  hour = STRMID(datetime_str,11,2) & minute = STRMID(datetime_str,14,2) & second = STRMID(datetime_str,17,2)

  geopack_recalc, year, month, date, hour, minute, second, tilt = tilt, /date

  IF model EQ 't89' THEN parmod = [(kp+1)<7.] ELSE parmod = [pdyn, Dst, imfBy, imfBz, 0., 0., tilt, xgsm, ygsm, zgsm]
  IF model EQ 't89' THEN t89 = 1 
  IF model EQ 't96' THEN t96 = 1
  IF model EQ 't01' THEN t01 = 1
  IF model EQ 'ts04' THEN ts04 = 1
  
  dir = 1
  geopack_trace, xgsm, ygsm, zgsm, dir, parmod, xf, yf, zf, fline = fline, tilt = tilt, igrf = 1, t89 = t89, t96 = t96, t01 = t01, ts04 = ts04
  n = (SIZE(fline))[1]
  field_length_para = TOTAL(SQRT((fline[0:(n-2),0]-fline[1:(n-1),0])^2 + (fline[0:(n-2),1]-fline[1:(n-1),1])^2 + (fline[0:(n-2),2]-fline[1:(n-1),2])^2))
  
  dir = -1
  geopack_trace, xgsm, ygsm, zgsm, dir, parmod, xf, yf, zf, fline = fline, tilt = tilt, igrf = 1, t89 = t89, t96 = t96, t01 = t01, ts04 = ts04
  n = (SIZE(fline))[1]
  field_length_anti = TOTAL(SQRT((fline[0:(n-2),0]-fline[1:(n-1),0])^2 + (fline[0:(n-2),1]-fline[1:(n-1),1])^2 + (fline[0:(n-2),2]-fline[1:(n-1),2])^2))
  
  RETURN,[field_length_para, field_length_anti]

END
