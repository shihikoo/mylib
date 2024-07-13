;+
;FUNCTION: time_string(time)
;NAME:
;  time_string
;PURPOSE:
;  Converts time to a date string.
;INPUT:  input can be a scaler or array of any dimension of type:
;  double(s)      seconds since 1970
;  string(s)      format:  YYYY-MM-DD/hh:mm:ss
;  structure(s)   format:  given in "time_struct"
;  float(s)
;  longs(s)
;                 values outside normal range will be corrected.
;KEYWORDS: 
;  FORMAT:         specifies output format.
;    FORMAT=0:  YYYY-MM-DD/hh:mm:ss
;    FORMAT=1:  YYYY Mon dd hhmm:ss
;    FORMAT=2:  YYYYMMDD_hhmmss
;    FORMAT=3:  YYYY MM dd hhmm:ss
;  SQL:            produces output format: "YYYY-MM-DD hh:mm:ss.sss"
;                  (quotes included) which convenient for building SQL queries.
;  PRECISION:      specifies precision
;      -5:   Year only
;      -4:   Year, month
;      -3:   Year, month, date
;      -2:   Year, month, date, hour
;      -1:   Year, month, date, hour, minute
;       0:   Year, month, date, hour, minute, sec
;      >0:   millisecs
;  AUTOPREC  If set PREC will automatically be set based on the array of times
;  DATE_ONLY:   Same as PREC = -3
;  MSEC:        Same as PREC = 3
;        
;OUTPUT:
;  string with the following format: YYYY-MM-DD/hh:mm:ss (Unless
;  modified by keywords.)
;
;See Also:  "time_double"  , "time_struct" or "time_ticks"
;
;NOTE:
;  This routine works on vectors and is designed to be fast.
;  Output will have the same dimensions as the input.
;
;CREATED BY:	Davin Larson  Oct 1996
;FILE:  time_string.pro
;VERSION:  1.11
;LAST MODIFICATION:  00/07/05
;-
function time_string,time0, $
   format = format,precision=prec,epoch=epoch,date_only=date_only, $
   msec = msec, sql=sql, autoprec=autoprec

ms=['','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

if keyword_set(msec) then prec=3

if data_type(time0) eq 0 then begin
   s= ''
   print,'Enter time(s)  (YYYY-MM-DD/hh:mm:ss)  blank line to quit:'
   read,s
   time0 = s
   while keyword_set(s) do begin
     read,s
     if keyword_set(s) then time0=[time0,s]
   endwhile
endif

if data_type(time0) eq 4 and n_elements(prec) eq 0  then prec=-1

if data_type(time0) ne 8 then time = time_struct(time0,epoch=epoch) $  
else time = time0                               ; Force input into a structure

if ndimen(time0) eq 0 then res='' $
else res = make_array(value='',dim=dimen(time0))

if not keyword_set(format) then fmt = 0 else fmt = format

if keyword_set(sql) then begin 
;    message,/info,'the SQL keyword is a STUPID keyword!'
    fmt = 4
    prec = 3
end

case fmt of
  0:  f = '(i4.4,"-",i2.2,"-",i2.2,"/",i2.2,":",i2.2,":",i2.2)'
  1:  f = '(i4.4," ",a," ",i2.2," ",i2.2,i2.2,":",i2.2)'
  2:  f = '(i4.4,i2.2,i2.2,"_",i2.2,i2.2,i2.2)'
  3:  f = '(i4.4," ",i2.2," ",i2.2," ",i2.2," ",i2.2," ",i2.2)'
  4:  f = '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2)'
endcase

if keyword_set(autoprec) then begin
   if n_elements(time) ge 1 then begin
      td = time_double(time)
      td = td[sort(td)]
      dt = min(abs(td-shift(td,1)))
   endif
   if dt le 0 then dt=1
   prec = -5
   if dt lt 364*86400. then prec= -4  ;months
   if dt lt  60*86400. then prec= -3  ;days
;   if dt lt     86400. then prec= -2  ;hours
   if dt lt  12* 3600. then prec= -1  ;min
   if dt lt        60. then prec= 0   ;sec
   if dt lt         1. then prec =  floor(1-alog10(dt))
endif


if keyword_set(date_only) then prec = -3
   
if keyword_set(prec) then begin
   posits = [[16,13,10,7,4],[16,14,11,8,4],[13,11,8,6,4],[16,13,10,7,4],[16,13,10,7,4]]
   if prec gt 0 then  pos = prec else pos= -posits[-prec-1,fmt]
endif

if data_type(time) eq 8 then begin       ; input is a structure
  for i=0l,n_elements(time)-1 do begin  ;Jing: change i from int to long
    t = time[i]
    case fmt of
        0:  s = string(form=f,t.year,t.month,t.date,t.hour,t.min,t.sec)
        1:  s = string(form=f,t.year,ms[t.month],t.date,t.hour,t.min,t.sec)
        2:  s = string(form=f,t.year,t.month,t.date,t.hour,t.min,t.sec)
        3:  s = string(form=f,t.year,t.month,t.date,t.hour,t.min,t.sec)
        4:  s = string(form=f,t.year,t.month,t.date,t.hour,t.min,t.sec)
    endcase
    if keyword_set(pos) then begin
       if pos gt 0 then s = s + strmid(string(t.fsec,format="(f16.14)"),1,pos+1)
       if pos lt 0 then s = strmid(s,0,-pos)
    endif
    res[i] = s
  endfor
  if keyword_set(sql) then begin
      res = '"' + res + '"'
  end
  notgood = where(finite(time.sod) eq 0)
  if (notgood[0] ne -1) then res[notgood] = 'NULL'
  return,res 
endif

message,/info,'Improper time input'

end

