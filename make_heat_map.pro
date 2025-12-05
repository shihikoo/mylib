pro draw_heat_map, x_axis, y_axis, data, no_interp = no_interp, title = title, xtitle = xtitle, ytitle = ytitle, ztitle = ztitle, xrange = xrange, yrange = yrange, zrange = zrange, xlog = xlog, ylog = ylog, zlog = zlog, ps_plot = ps_plot, filename = filename,remove_ps = remove_ps
  if ~keyword_set(no_interp) then no_interp = 1
  if ~keyword_set(title) then title = ''
  if ~keyword_set(xtitle) then xtitle = ''
  if ~keyword_set(ytitle) then ytitle = ''
  if ~keyword_set(ztitle) then ztitle = ''
  if ~keyword_set(xrange) then xrange = [-20., 20.]
  if ~keyword_set(yrange) then yrange = [20., -20.]
  if ~keyword_set(zrange) then zrange = [0., 1.]
  if ~keyword_set(zlog) then zlog = 0
  if ~keyword_set(filename) then filename = 'heat_map.ps'

  if keyword_set(ps_plot) then popen, filename, /land

  specplot, x_axis, y_axis, data, no_interp = 1, $
    lim = { $
      zlog: zlog, zrange: zrange $
      , title: title, xtitle: xtitle, ytitle: ytitle, ztitle: ztitle $
      , xrange: xrange, yrange: yrange, xlog: xlog, ylog: ylog $
      , zticklen: -1, xstyle: 1, ystyle: 1, charsize: 1.2 $
      , xcharSize: 1.5, ycharsize: 1.5, zcharsize: 1.5 $
      , position: [0.12, 0.12, 0.8, 0.8], zticks: 4}

  oplot, [0, 0, -100, 100], [-100, 100, 0, 0]

  if keyword_set(ps_plot) then pclose else stop
end

pro draw_mlt_heat_map, x_axis, y_axis, data, title = title, xtitle = xtitle, ytitle = ytitle, ztitle = ztitle, r_range = r_range, zrange = zrange, zlog = zlog, ps_plot = ps_plot, filename = filename,remove_ps = remove_ps
  if ~keyword_set(title) then title = ''
  if ~keyword_set(ztitle) then ztitle = ''
  if ~keyword_set(r_range) then r_range = []
  if ~keyword_set(zrange) then zrange = []
  if ~keyword_set(zlog) then zlog = 0
  if ~keyword_set(filename) then filename = 'heat_map.ps'

  norm_factor_mlt = 1 / 24. * 360 / 180 * !pi

  if keyword_set(ps_plot) then popen, filename, /land

  polar_spec, y_axis, x_axis * norm_factor_mlt, transpose(data) $
    , title = title, ztitle = ztitle $
    , r_range = r_range, zrange = zrange, zlog = zlog $
    , charsize = 1.5, zticklen = -1, zticks = 4 $
    , xtitle = '', ytitle = '', label_charsize = 2

  r_grid = abs(y_axis[1] - y_axis[0])
  mlt_grid = 30. / 180. * !pi
  full_mlt = 360. / 180. * !pi

  for i = 0, (full_mlt / mlt_grid) - 1 do oplot, r_range, [i * mlt_grid, i * mlt_grid], /polar

  ; plot_r_grid = 1.
  for j = 0, (max(r_range) / r_grid) - 1 do oplot, replicate(r_grid * (j + 1), 360), indgen(360) * !pi / 180., /polar

  oplot, replicate(8, 360), indgen(360) * !pi / 180., thick=4, /polar

  ; plot_r_grid = 6.
  ; for j = 0, (max(r_range) / plot_r_grid) - 1 do oplot, replicate(plot_r_grid * (j + 1), 360), indgen(360) * !pi / 180., thick=4, /polar

  if keyword_set(ps_plot) then pclose else stop
end

; --------------------------------------------------
; Purpose: make 2D heat map
; Inputs: x_axis, y_axis, data, filepath, region, ts_date, te_date,
; plot_axis, sort_title, phase
; Keywords: unit, xrange, yrange, zrange, zlog
; ---------------------------------------------------
pro make_heat_map, x_axis, y_axis, data, filename, title, plot_axis, unit = unit, xrange = xrange, yrange = yrange, zrange = zrange, xlog = xlog, ylog = ylog, zlog = zlog, ps_plot = ps_plot,remove_ps = remove_ps
  xtitle = plot_axis[0]
  ytitle = plot_axis[1]
  ztitle = unit
  r_range = yrange
  no_interp = 1
  
  spawn, 'mkdir -p ' + file_dirname(filename)
  title = title + ' avg:' + string(mean(data, /nan), format = '(f8.3)')

  if array_equal(plot_axis[0], ['MLT']) then draw_mlt_heat_map, x_axis, y_axis, data, title = title, xtitle = xtitle, ytitle = ytitle, ztitle = ztitle, r_range = r_range, zrange = zrange, zlog = zlog, ps_plot = ps_plot, filename = filename  else draw_heat_map, x_axis, y_axis, data, no_interp = no_interp, title = title, xtitle = xtitle, ytitle = ytitle, ztitle = ztitle, xrange = xrange, yrange = yrange, zrange = zrange, xlog = xlog, ylog = ylog, zlog = zlog, ps_plot = ps_plot, filename = filename
  
  if keyword_set(ps_plot) then begin
    png_filename = strmid(filename, 0, strpos(filename, '.ps')) + '.png'
    spawn, 'mogrify -format png -alpha opaque -density 150 ' + filename
    spawn, 'mogrify -rotate -90 ' + png_filename
    if keyword_set(remove_ps) then spawn, 'rm -f ' + filename
  endif
end