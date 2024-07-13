PRO draw_mlt_points_plot, data_pos, flag_para, flag_anti, filename = filename, title=title, xtitle = xtitle, ytitle = ytitle, xrange = xrange, yrange = yrange, xlog = xlog, ylog = ylog, ps_plot = ps_plot, grid=grid
;constants
  norm_factor_mlt=1/24.*360/180*!PI
  la_x = 20 & la_y = -20  

; default keywords settings
  IF KEYWORD_SET(ps_plot) AND ~KEYWORD_SET(filename) THEN filename = 'sampling_map.ps'
  IF ~KEYWORD_SET(grid) THEN grid = 1.
   
; basice settings 
  data_pos_mlt =  norm_factor_mlt * data_pos(*,0)
  data_pos_ilat = data_pos(*,1)
 
  r_range = ABS(yrange(0) - yrange(1))

; open the ps plot
  IF KEYWORD_SET(ps_plot) THEN popen, filename, /land 

  PLOT, [0, 0, -100, 100], [-100, 100, 0, 0] $
        , title = title, xtitle = xtitle, ytitle = ytitle, xrange = [-r_range,r_range], yrange = [-r_range, r_range] $
        , XSTYLE = 5, ystyle = 5, charsize = 1.2, position = [0.15, 0.15, 0.85, 0.85], xlog = xlog, ylog = ylog, xcharSIZE = 2, ycharsize = 2
  
  IF keyword_set(ps_plot) then psym_point_map = 1 else psym_point_map = 3

; plot data that has no beam
  oplot, data_pos_ilat*(flag_para EQ 0 AND flag_anti EQ 0), data_pos_mlt*(flag_para EQ 0 AND flag_anti EQ 0), color = 2, psym = psym_point_map, /polar

; plot data with beams
  oplot, data_pos_ilat*(ABS(flag_para) GT 0 OR ABS(flag_anti) GT 0), data_pos_mlt*(ABS(flag_para) GT 0 OR ABS(flag_anti) GT 0), psym = psym_point_map, color = 2, /polar
  
; legend         
;  xyouts, la_x+7, la_y+4.5*grid, 'no events', color = 2
;  xyouts, la_x+7, la_y, 'beam events', color = 2, charsize = 2

  xyouts, r_range, 0, '0', charsize = 2
  xyouts, 0, r_range, '6', charsize = 2
  xyouts, -r_range*1.05,0, '12', charsize = 2
  xyouts, 0 ,-r_range*1.05, '18', charsize = 2

; draw the grid lines
  for i=0, 11 do oplot,[0,r_range],[i*30./180.*!PI,i*30./180.*!PI],/polar
  for j=0, (r_range/10)-1 do  oplot, replicate(10*(j+1),360),indgen(360)*!PI/180.,/polar

; close the ps plot
  IF KEYWORD_SET(ps_plot) THEN pclose ELSE stop

END

PRO draw_points_plot, data_pos, flag_para, flag_anti, filename = filename, title=title, xtitle = xtitle, ytitle = ytitle, xrange = xrange, yrange = yrange, xlog = xlog, ylog = ylog, ps_plot = ps_plot, grid=grid
; default keywords settings
  IF KEYWORD_SET(ps_plot) AND ~KEYWORD_SET(filename) THEN filename = 'sampling_map.ps'
  IF ~KEYWORD_SET(grid) THEN grid = 1.

  la_x = 20 & la_y = -20        ; legend position

; open the ps plot
  IF KEYWORD_SET(ps_plot) THEN popen, filename, /land 

  PLOT, [0, 0, -100, 100], [-100, 100, 0, 0] $
        , title = title, xtitle = xtitle, ytitle = ytitle, xrange = xrange, yrange = yrange, xstyle = 1, ystyle = 1 $
        , charsize = 1.2, position = [0.15, 0.15, 0.85, 0.85], xlog = xlog, ylog = ylog, xcharsize = 2, ycharsize = 2

  if keyword_set(ps_plot) then psym_point_map=1 else psym_point_map=3

; plot data without beams      
  oplot, data_pos(*, 0)*(flag_para EQ 0 AND flag_anti EQ 0), data_pos(*, 1)*(flag_para EQ 0 AND flag_anti EQ 0), symsize=0.5, psym = psym_point_map, color = 2
; plot data with beams
  oplot, data_pos(*, 0)*(ABS(flag_para) GT 0 OR ABS(flag_anti) GT 0), data_pos(*, 1)*(ABS(flag_para) GT 0 OR ABS(flag_anti) GT 0), symsize=0.5, psym = psym_point_map, color = 2
; legend         
;  xyouts, la_x, la_y+4.5*grid, 'no events', color = 5, charsize = 2
;  xyouts, la_x, la_y, 'beam evnets', color = 2, charsize = 2

; drow the center lines 
  OPLOT, [0, 0, -100, 100], [-100, 100, 0, 0], col = 0, thick = 8
  IF KEYWORD_SET(ps_plot) THEN pclose ELSE stop
 
END 

PRO make_points_map, data_pos, flag_para, flag_anti, beta, mlt, range, log, path_ps, ts_date, te_date, plot_axis, grid,  ext_condition_str, int_condition_str, ps_plot = ps_plot

  xtitle = PLOT_AXIS(0) 
  ytitle = PLOT_AXIS(1)  
   
  xrange = range(*,0)
  yrange = range(*,1)
  xlog = log(0)
  ylog = log(1)

  filename =  path_ps + 'footage/' + ext_condition_str + '_' + int_condition_str + '_sampling_' + ts_date+'_to_'+ te_date+'_' + PLOT_AXIS(0)+'_vs_'+PLOT_AXIS(1) +'.ps'
  
  title =  ext_condition_str+'!C'+int_condition_str +'!Cfrom ' +ts_date+' to ' +te_date
  
  spawn, 'mkdir -p ' + FILE_DIRNAME(filename)

  IF PLOT_AXIS(0) EQ 'MLT' THEN BEGIN 
     draw_mlt_points_plot, data_pos, flag_para, flag_anti, filename = filename, ps_plot = ps_plot, grid = grid, title=title, xtitle = xtitle, ytitle = ytitle, xrange = xrange, yrange = yrange, xlog = xlog, ylog = ylog
  ENDIF ELSE BEGIN
     draw_points_plot, data_pos, flag_para, flag_anti, filename = filename, ps_plot = ps_plot, grid = grid, title=title, xtitle = xtitle, ytitle = ytitle, xrange = xrange, yrange = yrange, xlog = xlog, ylog=ylog
  ENDELSE
  
  IF KEYWORD_SET(ps_plot) THEN BEGIN 
     png_filename = STRMID(filename, 0, STRPOS(filename,'.ps')) + '.png'  
     spawn, 'mogrify -format png -alpha opaque -density 150 '+ filename
     spawn, 'mogrify -rotate -90 '+ png_filename
;     spawn, 'rm -f ' + filename      
  ENDIF 

END  
