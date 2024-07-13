FUNCTION r_data_with_timetrim, var, x=x, y=y, v=v, ts = ts, te= te, time = time

COMMON get_error, get_err_no, get_err_msg, default_verbose

tplot_names, var, names = names
IF names EQ '' THEN BEGIN

    get_err_no = 1
    get_err_msg = 'Variable: ' + var  + ' does not exist'
    RETURN, 0

ENDIF

get_data, var, data = data

IF SIZE(data, /TYPE) EQ 8 THEN BEGIN
    IF KEYWORD_SET(X) THEN output = data.x
    IF KEYWORD_SET(Y) THEN  output = data.y
    IF KEYWORD_SET(V) THEN  output = data.v

    if keyword_set(ts) and keyword_set(te) then begin 
        index = where(time GE ts and time LE te, ct)
        if ct gt 0 then begin
            outputsize = size(output)
            if outputsize[0] eq 1 then output = output[index] $
            else if outputsize[0] eq 2 then output = output[index,*] $
            else if outputsize[0] eq 3 then output = output[index,*,*] $
            else stop
        endif else begin 
            get_err_msg = 'Variable: ' + var  + "wrong time range. zero content"
            get_err_no = 1
            RETURN, 0
        endelse  

        return, output
    endif else begin
        return, output
    endelse 
ENDIF  ELSE BEGIN
    
    get_err_no = 1
    RETURN, 0

ENDELSE

END 