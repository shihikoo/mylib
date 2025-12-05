; keyword keep_v as it is
PRO time_trim_tplot_variable, tplot_name, t_s, t_e, keep_v = keep_v
  tplot_names, tplot_name, names=names
  IF names[0] EQ '' THEN RETURN

  FOR iname = 0, N_ELEMENTS(names)-1 DO BEGIN 
     get_data,names[iname], data=data
     index = where(data.x GE t_s AND data.x LE t_e, ct)
     IF ct GT 0 THEN BEGIN
        str_element, data, 'X', value, SUCCESS=xfound
        IF KEYWORD_SET(xfound) THEN data_x = value(index) 
        
        str_element, data, 'Y', value, SUCCESS=yfound
        IF KEYWORD_SET(yfound) THEN data_y = value(index, *, *)
        
        str_element, data, 'V', value, SUCCESS=vfound
        IF KEYWORD_SET(vfound) then begin
           if ~keyword_set(keep_v) THEN data_v = value(index, *, *) else  data_v = value
        endif 
        str_element, data, 'dY', value, SUCCESS=dyfound
        IF KEYWORD_SET(dyfound) THEN data_dy = value(index, *,*)

        str_element, data, 'dv', value, SUCCESS=dvfound
        IF KEYWORD_SET(dvfound) THEN data_dv = value(index, *,*) else  data_v = value

     ENDIF ELSE BEGIN
        data_x = !VALUES.F_NAN
        data_y = !VALUES.F_NAN
        data_v = !VALUES.F_NAN
        data_dy = !VALUES.F_NAN
     ENDELSE 
     
     datastr = {x:data_x, y:data_y}

     IF KEYWORD_SET(vfound) then datastr=CREATE_STRUCT(datastr, 'v',data_v )
     IF KEYWORD_SET(dyfound) then datastr=CREATE_STRUCT(datastr, 'dy',data_dy )
     IF KEYWORD_SET(dvfound) then datastr=CREATE_STRUCT(datastr, 'dv',data_dv )

   ;   IF NOT KEYWORD_SET(vfound) AND NOT KEYWORD_SET(dyfound) THEN datastr = {x:data_x, y:data_y}
   ;   IF KEYWORD_SET(vfound) AND NOT KEYWORD_SET(dyfound) THEN datastr = {x:data_x, y:data_y, v:data_v}
   ;   IF NOT KEYWORD_SET(vfound) AND KEYWORD_SET(dyfound) THEN datastr = {x:data_x, y:data_y, dy:data_dy}
   ;   IF KEYWORD_SET(vfound) AND KEYWORD_SET(dyfound) THEN datastr = {x:data_x, y:data_y, v:data_v, dy:data_dy}
     
     str_element, data, 'energybins', value, SUCCESS=ebfound
     IF ebfound EQ 1 THEN datastr = {x: data_x, y:data_y, energybins:data.energybins}

     str_element, data, 'average_time', value, SUCCESS=beamFound
     IF beamFound EQ 1 THEN BEGIN
        datastr = {x: data_x, y:data_y, v:data_v, start_time:data.start_time, END_time: data.end_time, average_time: data.average_time}
     ENDIF 
  store_data, names[iname], data= datastr
  ENDFOR 
END
