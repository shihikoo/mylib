PRO draw_heat_map, x_axis, y_axis, data, no_interp = no_interp, title = title, xtitle = xtitle, ytitle = ytitle, ztitle = ztitle, xrange = xrange, yrange = yrange, zrange = zrange, xlog = xlog, ylog = ylog, zlog = zlog, ps_plot = ps_plot, filename = filename

  IF ~KEYWORD_SET(no_interp) THEN no_interp = 1
  IF ~KEYWORD_SET(title) THEN title = ''
  IF ~KEYWORD_SET(xtitle) THEN xtitle = ''
  IF ~KEYWORD_SET(ytitle) THEN ytitle = ''
  IF ~KEYWORD_SET(ztitle) THEN ztitle = ''
  IF ~KEYWORD_SET(xrange) THEN xrange = [-20., 20.]
  IF ~KEYWORD_SET(yrange) THEN yrange = [20.,-20.]
  IF ~KEYWORD_SET(zrange) THEN zrange = [0., 1.]
  IF ~KEYWORD_SET(zlog) THEN zlog = 0
  IF ~KEYWORD_SET(filename) THEN filename = 'heat_map.ps'

  IF KEYWORD_SET(ps_plot) THEN popen, filename, /land 
  
  specplot, x_axis, y_axis, data, no_interp = 1, $
            lim = {  $
            zlog:zlog, zrange:zrange $
            , title:title, xtitle:xtitle, ytitle:ytitle, ztitle:ztitle $
            , xrange:xrange, yrange:yrange, xlog:xlog, ylog:ylog $
            , zticklen: -1, XSTYLE:1, ystyle: 1, charsize: 1.2 $
            , xcharSIZE: 1.5, ycharsize: 1.5, zcharsize:1.5 $
            , position: [0.12, 0.12, 0.8, 0.8], zticks: 4}     
  
  oplot, [0, 0, -100, 100], [-100, 100, 0, 0]
  
  IF KEYWORD_SET(ps_plot) THEN pclose  ELSE stop
END

PRO draw_mlt_heat_map, x_axis, y_axis, data, title = title, xtitle = xtitle, ytitle = ytitle, ztitle = ztitle, r_range = r_range, zrange = zrange,zlog = zlog, ps_plot = ps_plot, filename = filename

  IF ~KEYWORD_SET(title) THEN title = ''
  IF ~KEYWORD_SET(ztitle) THEN ztitle = ''
  IF ~KEYWORD_SET(r_range) THEN r_range = []
  IF ~KEYWORD_SET(zrange) THEN zrange = []
  IF ~KEYWORD_SET(zlog) THEN zlog = 0
  IF ~KEYWORD_SET(filename) THEN filename = 'heat_map.ps'

  norm_factor_mlt=1/24.*360/180*!PI
  data = transpose(data)

  IF KEYWORD_SET(ps_plot) THEN popen, filename, /land 
  
  polar_spec, y_axis, x_axis*norm_factor_mlt, data $
              , title = title, ztitle = ztitle $
              , r_range= r_range, zrange = zrange, zlog = zlog $
              , charsize = 1.5, zticklen = -1, zticks = 4 $ 
              , xtitle = '', ytitle = '', label_charsize = 2
  
  r_grid = ABS(y_axis[1]-y_axis[0])
  mlt_grid = 30./180.*!PI
  full_mlt = 360./180.*!PI

  for i=0, (full_mlt/mlt_grid) -1 do oplot, r_range,[i*mlt_grid, i*mlt_grid],/polar
  for j=0, (max(r_range)/r_grid)-1 do oplot, replicate(r_grid*(j+1),360), indgen(360)*!PI/180.,/polar

  IF KEYWORD_SET(ps_plot) THEN    pclose  ELSE stop

END 

;--------------------------------------------------
; Purpose: make 2D heat map
; Inputs: x_axis, y_axis, data, filepath, region, ts_date, te_date,
; plot_axis, sort_title, phase
; Keywords: unit, xrange, yrange, zrange, zlog
;---------------------------------------------------
PRO make_heat_map, x_axis, y_axis, data, filename, title, plot_axis, unit = unit, xrange = xrange, yrange = yrange,zrange=zrange, xlog = xlog, ylog = ylog, zlog = zlog, ps_plot = ps_plot

  xtitle = plot_axis(0)
  ytitle = plot_axis(1)
  ztitle = unit
  r_range = yrange
  no_interp = 1

  spawn, 'mkdir -p ' + FILE_DIRNAME(filename)

  ind = where(FINITE(data),ct)
  if ct gt 0 then begin
     average_data = TOTAL(data,/nan)/ct
     title = title + ' avg:' + string(average_data,format='(f8.3)')
  endif 
  draw_heat_map, x_axis, y_axis, data, no_interp = no_interp, title = title, xtitle = xtitle, ytitle = ytitle, ztitle = ztitle, xrange = xrange, yrange = yrange, zrange = zrange, xlog = xlog, ylog = ylog, zlog = zlog, ps_plot = ps_plot, filename = filename
  
  IF ARRAY_EQUAL(PLOT_AXIS[0], ['MLT']) THEN draw_mlt_heat_map, x_axis, y_axis, data, title = title, xtitle = xtitle, ytitle = ytitle, ztitle = ztitle, r_range = r_range, zrange = zrange, zlog = zlog, ps_plot = ps_plot, filename = filename
  
  IF KEYWORD_SET(ps_plot) THEN BEGIN   
     png_filename = STRMID(filename, 0, STRPOS(filename,'.ps')) + '.png'    
     spawn, 'mogrify -format png -alpha opaque -density 150 '+ filename
     spawn, 'mogrify -rotate -90 '+ png_filename 
;     spawn, 'rm -f ' + filename  
  ENDIF 

END 
