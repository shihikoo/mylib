;+
;PROCEDURE specplot,x,y,z
;NAME:
;   specplot
;PURPOSE:
;   Creates a spectrogram plot.
;   All plot limits and plot positions are handled by the keyword LIMITS.
;INPUT:  
;   x:  xaxis values:  dimension N.
;   y:  yaxis values:  dimension M.  (Future update will allow (N,M))
;   Z:  color axis values:  dimension (N,M).
; 
;   All options are passed in through a single structure.
;KEYWORDS:
;   LIMITS:  A structure that may contain any combination of the following 
;       elements:
;       X_NO_INTERP:   Prevents interpolation along the x-axis.
;       Y_NO_INTERP:   Prevents interpolation along the y-axis.
;       NO_COLOR_SCALE: Prevents drawing of color bar scale.
;       BOTTOM, TOP:   Sets the bottom and top colors for byte-scaling
;       ALL plot keywords such as:  
;       XLOG,   YLOG,   ZLOG,
;       XRANGE, YRANGE, ZRANGE,
;       XTITLE, YTITLE,
;       TITLE, POSITION, REGION  etc. (see IDL documentation for a description)
;         The following elements can be included in LIMITS to effect DRAW_COLOR_SCALE:
;       ZTICKS, ZRANGE, ZTITLE, ZPOSITION, ZOFFSET
;   DATA:  A structure that provides an alternate means of supplying 
;       the data and options.  This is the method used by "TPLOT".
;   X_NO_INTERP:   Prevents interpolation along the x-axis.
;   Y_NO_INTERP:   Prevents interpolation along the y-axis.
;   OVERPLOT:      If non-zero then data is plotted over last plot.
;   OVERLAY:	   If non-zero then data is plotted on top of data from last
;		   last plot.
;   PS_RESOLUTION: Post Script resolution.  Default is 60.
;   NO_INTERP:	   If set, do no x or y interpolation.
;   IGNORE_NAN:    If nonzero, ignore data points that are not finite.
;      
;Notes:
;  - The arrays x and y MUST be monotonic!  (increasing or decreasing)
;  - The default is to interpolate in both the x and y dimensions.
;  - Data gaps can be included by setting the z values to NAN  (!values.f_nan).
;  - If ZLOG is set then non-positive zvalues are treated as missing data.
;
;See Also:  "XLIM", "YLIM", "ZLIM",  "OPTIONS",  "TPLOT", "DRAW_COLOR_SCALE"
;-
pro specplot,x,y,z,limits=lim,data=data,overplot=overplot,overlay=overlay,$
	ps_resolution=ps_res,x_no_interp=x_no_interp,y_no_interp=y_no_interp, $
        no_interp=no_interp,ignore_nan=ignore_nan

opt = {xrange:[0.,0.],yrange:[0.,0.],zrange:[1.,1.]}

if keyword_set(data) then begin
  x = data.x
  y = data.v
  z = data.y
  extract_tags,opt,data,except=['x','y','v']
  str_element,lim,'datagap',dg
  if keyword_set(dg) then makegap,dg,x,z,v=y
endif

if keyword_set(no_interp) then begin
   x_no_interp=1
   y_no_interp=1
endif

if n_params() eq 1 then begin
  dim = dimen(x)
  specplot,findgen(dim(0)),findgen(dim(1)),x,limits=lim,overplot=overplot,$
       overlay=overlay,ps_resolution=ps_res, $
       x_no_interp=x_no_interp,y_no_interp=y_no_interp
  return
endif

extract_tags,opt,lim

if opt.xrange(0) eq opt.xrange(1) then opt.xrange = minmax_range(x)
if opt.yrange(0) eq opt.yrange(1) then opt.yrange = minmax_range(y)

;str_element,opt,'ytype',value=ylog   ; obsolete keywords
;str_element,opt,'xtype',value=xlog
;str_element,opt,'ztype',value=zlog

str_element,opt,'xlog',value=xlog
str_element,opt,'ylog',value=ylog
str_element,opt,'zlog',value=zlog

str_element,opt,'gifplot',value=gifplot
if keyword_set(gifplot) then begin
  x_no_interp = 1
  y_no_interp = 1
  no_color_scale = 1
endif

str_element,opt,'x_no_interp',value=x_no_interp
str_element,opt,'y_no_interp',value=y_no_interp
str_element,opt,'no_interp',value=no_interp
if keyword_set(no_interp) then begin
   x_no_interp=1
   y_no_interp=1
endif


str_element,opt,'max_value',value=mx
str_element,opt,'min_value',value=mn

;if keyword_set(mx) then print,'max_value= ', mx

str_element,opt,'ztitle',value=ztitle

if not keyword_set(overplot) then box,opt     ; Sets plot parameters.


zrange = opt.zrange
y1 = y
if keyword_set(ylog) then begin
  bad = where( finite(y1) eq 0, c)
  if c ne 0 then y1(bad) = 0.
  bad = where(y1 le 0,c)
  if c ne 0 then y1(bad) = !values.f_nan
  y1 = alog10(y1)
endif

if keyword_set(xlog) then x1 = alog10(x) else x1 = x

z1 = z
if keyword_set(zlog) then begin
   bad = where( finite(z1) eq 0, c)
   if c ne 0 then z1(bad) = 0.
   bad = where(z1 le 0,c)
   if c ne 0 then z1(bad) = !values.f_nan
   z1 = alog10(z1)
   zrange = alog10(zrange)
   if keyword_set(mn) then mn = alog10(mn)
   if keyword_set(mx) then mx = alog10(mx)
endif

xwindow=!x.window
ywindow=!y.window
xcrange=!x.crange
ycrange=!y.crange

str_element,opt,'overlay',value=overlay
if keyword_set(overlay) then begin
   winpos = convert_coord(minmax_range(x),minmax_range(y),/data,/to_norm)
   xwr = minmax_range(winpos(0,*))
   ywr = minmax_range(winpos(1,*))
   xwindow(0) = xwindow(0) > xwr(0)
   xwindow(1) = xwindow(1) < xwr(1)
   ywindow(0) = ywindow(0) > ywr(0)
   ywindow(1) = ywindow(1) < ywr(1)
   datpos = convert_coord(xwindow,ywindow,/norm,/to_data)
   xcrange = reform(datpos(0,*))
   ycrange = reform(datpos(1,*))
   if !x.type then xcrange = alog10(xcrange)
   if !y.type then ycrange = alog10(ycrange)
endif


pixpos = round(convert_coord(xwindow,ywindow,/norm,/to_device))
npx = pixpos(0,1)-pixpos(0,0)+1
npy = pixpos(1,1)-pixpos(1,0)+1
xposition = pixpos(0,0)
yposition = pixpos(1,0)

str_element,opt,'ignore_nan',ignore_nan
if keyword_set(ignore_nan) then begin
   wg = where(finite(total(z1,2)),c)
   if c gt 0 then begin 
     z1 = z1[wg,*]
     y1 = y1[wg,*]
     x1 = x1[wg]
   endif
endif

if !d.flags and 1 then begin   ; scalable pixels (postscript)
   if keyword_set(ps_res) then ps_resolution=ps_res else  ps_resolution = 60.  ; Postscript defaults to 60 dpi
   str_element,opt,'ps_resolution',value=ps_resolution
   scale = ps_resolution/!d.x_px_cm/2.54
endif else scale = 1.

yd = ndimen(y1)
if yd eq 1 then begin            ; Typical, y does not vary with time
  nypix = round(scale*npy)
  ny = n_elements(y1)
  yp = findgen(nypix)*(ycrange(1)-ycrange(0))/(nypix-1) + ycrange(0)
  ys = interp(findgen(ny),y1,yp)
  if keyword_set(y_no_interp) then  ys = round(ys)

  nxpix = round(scale*npx)
  nx = n_elements(x1)
  xp = findgen(nxpix)*(xcrange(1)-xcrange(0))/(nxpix-1) + xcrange(0)
  xs = interp(findgen(nx),x1,xp )
  if keyword_set(x_no_interp) then  xs = round(xs)
  image = interpolate(z1,xs,ys,missing = !values.f_nan,/grid)
  
  str_element,opt,'roi',roi
  if keyword_set(roi) then begin
     xp_ = xp # replicate(1.,nypix)
     yp_ = replicate(1.,nxpix) # yp 
     roi_x = keyword_set(xlog) ? alog10(roi[*,0]) : roi[*,0]
     roi_y = keyword_set(ylog) ? alog10(roi[*,1]) : roi[*,1]
     dummy = enclosed(xp_,yp_,roi_x,roi_y,ncircs=ncirc)
     image[where(ncirc eq 0)] = !values.f_nan  
  endif
  
endif else begin
;  starttime = systime(1)
;  message,'y is 2 dimensional.  Please be patient...',/info

  nypix = round(scale*npy)
  ny = dimen2(y1)
  yp = findgen(nypix)*(ycrange(1)-ycrange(0))/(nypix-1) + ycrange(0)
  nxpix = round(scale*npx)

  nx = n_elements(x1)
  xp = findgen(nxpix)*(xcrange(1)-xcrange(0))/(nxpix-1) + xcrange(0)
  xs = interp(findgen(nx),x1,xp)
  xs = xs # replicate(1.,nypix)
  bad = where(finite(xs) eq 0,c)
  if c ne 0 then xs(bad)=-1
  if keyword_set(x_no_interp) then  xs = round(xs)

  ys = replicate(-1.,nxpix,nypix)
  ny1 = dimen1(y1)
  y_ind = findgen(ny)
  xi = round(xs)
  for i=0l,nxpix-1 do begin
    m = (xi(i) > 0) < (ny1-1)
    yt1 = reform(y1(m,*))
    ys(i,*) = interp(y_ind,yt1,yp)
  endfor
;dtime = systime(1)-starttime
;message,string(dtime)+' seconds.',/info

  bad = where(finite(ys) eq 0,c)
  if c ne 0 then ys(bad)=-1
  if keyword_set(y_no_interp) then  ys = round(ys)
  image = interpolate(z1,xs,ys,missing = !values.f_nan)

endelse

str_element,opt,'bottom',value=bottom
str_element,opt,'top',   value=top

if not keyword_set(gifplot) then begin

  if zrange(0) eq zrange(1) then $
     zrange = minmax_range(image,max=mx,min=mn)
  image = bytescale(image,bottom=bottom,top=top,range=zrange)

endif

tv,image,xposition,yposition,xsize=npx,ysize=npy

;redraw the axes
add_str_element,opt,'noerase',1
add_str_element,opt,'overplot',/delete
add_str_element,opt,'ytitle',/delete
add_str_element,opt,'position',reform(transpose([[!x.window],[!y.window]]),4)
;help,opt,/st
box,opt

if keyword_set(zlog) then zrange = 10.^zrange

charsize=!p.charsize
str_element,opt,'charsize',value=charsize
if not keyword_set(charsize) then charsize = 1.

str_element,opt,'no_color_scale',value=no_color_scale
str_element,opt,'zticks',zticks
str_element,opt,'zposition',zposition
str_element,opt,'zoffset',zoffset
str_element,opt,'zticklen',zticklen ; Jing
if not keyword_set(no_color_scale) then $
  draw_color_scale,brange=[bottom,top],range=zrange,log=zlog,title=ztitle, $
    charsize=charsize,yticks=zticks,position=zposition,offset=zoffset,ticklen=zticklen ; Jing: add zticklen

end
