;-----------------------------------------------
; Purpose: The function export the unique values in a vector and
; number of occurance of that value.
; Inputs: vec, a vector
;
; Written by Jing Liao
; Written on 05/14/2021
;-----------------------------------------------

FUNCTION table, vector
  vec = vector

  index = WHERE(~FINITE(vec), inf_ct)

  index = WHERE(FINITE(vec), ct)
  IF ct GT 0 THEN vec = vec[index]
 
  values = vec[uniq(vec,sort(vec))]  
  nvalues = N_ELEMENTS(values)  
  output = STRARR(2, nvalues) 

  FOR i = 0, nvalues-1 DO BEGIN 
     value = values(i)
     index = where(vec EQ value, ct)
     output[0,i] = STRING(value)
     output[1,i] = STRING(ct)
  ENDFOR

  IF inf_ct GT 0 THEN BEGIN 
     output = [[output], [STRING(!VALUES.F_NAN), STRING(inf_ct)]]
  ENDIF 

  RETURN, TRANSPOSE(output)
END
