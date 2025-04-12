;+
; PROCEDURE: xline
;
; PURPOSE: Draws a vertical line as an overplot. Because it is an
;          overplot the tplot variable nust be plotted first, and it
;          has to be redrawn every time the draw window is
;          refreshed. Default is at start time of the time interval.
;
; INPUT: var -> tplot variable on which the yline will be plotted
;               over. It can be called either by number or name. If
;               var is an array the yline will be overplotted on all
;               tplot variables as long as they are plotted before the
;               xline routine os called.
;
; KEYWORDS: offset -> x value of the zero line. It can be an array of
;                     values but then it has to match the array size
;                     of the var variable
;           col -> change the color of the line by providing a color
;                  number (on my system 1:purple, 2:blue, 3:light
;                  blue, 4:green, 5:yellow, 6:red). It can be an array
;                  of values and it will cycle over the number of
;                  variables.
;
; CREATED BY: J. Liao (03/36/21)
;
;-
PRO xline, var, OFFSET=OFFSET, COL=COL, LINESTYLE=LINESTYLE, THICK=THICK
  
  tplot_names, var, names=names
  
  IF KEYWORD_SET(names) THEN BEGIN
  
    get_timespan,time_interval
    t_s=gettime(time_interval(0)) ; start time in tplot-time
    t_e=gettime(time_interval(1)) ; end time in tplot-time  
    
    jj = 0
    kk = 0
    ll = 0
    FOR ii = 0, N_ELEMENTS(names)-1 DO BEGIN
      name = names(ii)
      get_data, name, lim=lim
stop
      l_y = [lim.ylim(0), lim.ylim(1)]
      
      IF KEYWORD_SET(OFFSET) THEN BEGIN
        l_x = [offset, offset] 
      ENDIF ELSE l_y = [(t_s+t_e)/2, (t_s+t_e)/2]
      
      store_data, 'zero_line', data={x:l_x, y:l_y}, dlimit={labels:''}
      
      
      IF KEYWORD_SET(COL) THEN BEGIN
        options, 'zero_line', color=col(jj)
        jj = jj + 1
        IF jj EQ N_ELEMENTS(col) THEN jj = 0
      ENDIF
      
      IF KEYWORD_SET(LINESTYLE) THEN BEGIN
        options, 'zero_line', linestyle=linestyle
        kk = kk + 1
        IF kk EQ N_ELEMENTS(linestyle) THEN kk = 0
      ENDIF
      
      IF KEYWORD_SET(THICK) THEN BEGIN
        options, 'zero_line', thick=thick
        ll = ll + 1
        IF ll EQ N_ELEMENTS(linestyle) THEN ll = 0
      ENDIF
      
      tplot_panel, var=names(ii), oplot='zero_line'
      
      store_data, 'zero_line', /delete
      
    ENDFOR
  ENDIF ELSE BEGIN
    PRINT, 'pro xline: TPLOT Variable does not exist'
  ENDELSE
    
    
END
