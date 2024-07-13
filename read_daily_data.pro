;----------------------------------------------------------------
; Purpose: Read daily csv data files within the requested time interval into a
; matrix
; Inputs: jd_s, ndays, data_path
;
; Written by Jing Liao
; Written on 04/21/20211
;---------------------------------------------------------------
FUNCTION read_daily_csv_into_matrix_rbsp, jd_s, ndays, data_path, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019
  FOR iday = 0l, ndays-1 DO BEGIN          ; Loop trough all days   
     caldat, jd_s + iday, month, day, year ; find caledar date
   
     month_str = string(month, format = '(i2.2)')
     day_str = string(day, format = '(i2.2)')
     year_str = string(year, format = '(i4.4)')
     fln = data_path + 'storm_o_beam_'+year_str+month_str+day_str  +'.csv'

     start_date_double = time_double(year_str+'-'+month_str+'-'+day_str)
     if keyword_set(avoid_compression_time) and start_date_double ge time_double('2019-04-17') and start_date_double le time_double('2019-08-17') then continue
     if keyword_set(avoid_2019) and start_date_double ge time_double('2019-01-01') and start_date_double le time_double('2019-12-31') then continue
     
     names = FINDFILE(fln, count = ct)
     IF ct EQ 1 THEN BEGIN
        idata_structured = READ_CSV(names[0], HEADER = header)
        nterm = N_ELEMENTS(header)
        n_avg = N_ELEMENTS(idata_structured.FIELD01)
        idata = DBLARR(nterm, n_avg)
        FOR ifield = 1, nterm DO BEGIN
           str_element, idata_structured,'FIELD' $
                        + string(ifield,format='(i2.2)'), $
                        value, success = found
           IF found THEN idata[ifield-1,*] = value ELSE stop
        ENDFOR
        IF KEYWORD_SET(data) THEN data = [[data],[idata]] ELSE data = idata
     ENDIF
  ENDFOR
  
  ntime = N_ELEMENTS(data[0,*])
  IF ntime EQ 0 THEN BEGIN 
     print, 'no files found' 
     RETURN,0
  ENDIF 
  output =  {header: header, data:data}
  RETURN, output
END 

;-----------------------------------------------------------------------------------------
; Purpose: extract a column of data out from daily data matrix for the given column_name
; Inputs: data, headers, column_name
; Output: an array
;
; Written by Jing Liao
; Written on 04/21/2021
;-----------------------------------------------------------------------------------------
FUNCTION extract_column_from_matrix_rbsp, data, header, column_name
  index = where(header EQ column_name, ct)
  IF ct GT 1 OR ct EQ 0 THEN BEGIN
     print, 'Error: no time column store'
     stop
  ENDIF 
  output = REFORM(data(index,*))
  
  RETURN,output
END 

;----------------------------------------------------------------------------------------
;Purpose: convert data matrix into a structure data with column name
;as tags
;
;Inputs: header, data
;----------------------------------------------------------------------------------------
FUNCTION convert_daily_data_matrix_rbsp, data_matrix, header

  FOR icolumn = 0, N_ELEMENTS(header)-1 DO BEGIN
     column_name = header(icolumn)
     column_data = extract_column_from_matrix_rbsp(data_matrix, header, column_name)

     IF ~KEYWORD_SET(data) THEN data = CREATE_STRUCT(column_name, column_data) $
     ELSE data=CREATE_STRUCT(data, column_name, column_data)
;struct_add_field, data, column_name, column_data
  ENDFOR
  
  RETURN, data
END

;----------------------------------------------------------------------------------------
;Purpose clean up data matrix
;Inputs: data, header
;Output: data
;---------------------------------------------------------------------------------------
FUNCTION clean_up_daily_data_matrix_rbsp, data, header, ts, te
;-- convert all data into doubles
  data = DOUBLE(data)

;-- keep only the data within the range
  index_column = WHERE(header EQ 'Time', ct)
  index_row = WHERE(data[index_column,*] GE ts and data[index_column,*] LE te, ct)
  IF ct GT 0 THEN data = data[ *,index_row]

;-- set all infinite flag value to nan
  index_column = WHERE(header EQ 'Flag_para', ct)
  index_row = WHERE( ~FINITE(data[index_column,*]), ct)
  IF ct GT 0 THEN data[index_column,index_row] = !VALUES.F_NAN
  
  index_column = WHERE(header EQ 'Flag_anti', ct)
  index_row = WHERE( ~FINITE(data[index_column,*]), ct)
  IF ct GT 0 THEN data[index_column, index_row] = !VALUES.F_NAN

;-- require beta is valid
  index_column = WHERE(header EQ 'Beta', ct)
  index_row = WHERE(FINITE(data[index_column,*]), ct )
  IF ct GT 0 THEN data = data[ *,index_row]

;-- region needs to be within magnetosphere
  index_column = WHERE(header EQ 'Region', ct)
  index_row = WHERE(data[index_column,*] EQ 1, ct)
  IF ct GT 0 THEN data = data[*, index_row]

;-- Distance larger than 100 Re is placeholder for error data
  index_column = WHERE(header EQ 'DIST', ct)
  index_row = WHERE(data[index_column, *] GT 100., ct )
; This is a special index system. For some reason data[index_column,
; index_row] gives errors
  IF ct GT 0 THEN data[index_column, index_row, 0] = !VALUES.F_NAN

  RETURN, data
END 

;----------------------------------------------------------------------------------------
;Purpose: Read daily csv data 
;Inputs: data, header
;Output: data
;---------------------------------------------------------------------------------------
FUNCTION read_daily_csv_rbsp, jd_s, ndays, data_path,ts,te, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019
  raw_data = read_daily_csv_into_matrix_rbsp(jd_s, ndays, data_path, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019)
  data_matrix = raw_data.data
  header = raw_data.header

  data_matrix = clean_up_daily_data_matrix_rbsp(data_matrix, header, ts, te)

  data = convert_daily_data_matrix(data_matrix, header)

  output =  {data:data,header:header}
  return,output
END

;---------------------------------------------------------------------------------------------------------
; Purpose: Read the daily data and store into structure data
;
; Created by Jing Liao
; Created on 04/13/2021
;---------------------------------------------------------------------------------------------------------
FUNCTION read_daily_data, time_start, time_end, tplot_path, data_path, read_from_dat = read_from_dat,store_tplot=store_tplot, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019

;--- set the time info ---
  ts = time_double(time_start) &  te = time_double(time_end) & dt = te-ts
  
  timespan, time_start, dt,  /SECONDS

  ts_str = time_struct(ts) 
  te_str = time_struct(te)      ; time structure   
  jd_s = julday(ts_str.month, ts_str.date, ts_str.year) 
  jd_e = julday(te_str.month, te_str.date, te_str.year) ; julian day

  time_str = strcompress(ts_str.year, /remove_all) + string(ts_str.month, format = '(i2.2)') $
             + string(ts_str.date, format = '(i2.2)') + '_to_' $
             + strcompress(te_str.year, /remove_all)  +strcompress(te_str.month, /remove_all) $
             +strcompress(te_str.date, /remove_all)
; number of days to be loaded
  ndays = (jd_e - jd_s) + 1     
; Last day is not included if hour=min=sec=0
  IF te_str.hour EQ 0 AND te_str.min EQ 0 AND te_str.sec EQ 0 THEN ndays = ndays - 1

;--------------------------------------------------------------
; Read from prestore data if tplot file exists and keyword
; read_from_dat set to 0
;--------------------------------------------------------------
  fln_saved_tplot = tplot_path + 'fulldata_' + time_str
  
  PRINT, FINDFILE(fln_saved_tplot+'.tplot', COUNT = ct_tplot)
  IF ct_tplot GT 0 AND NOT  KEYWORD_SET(read_from_dat)  THEN BEGIN 
     TPLOT_RESTORE, filenames = fln_saved_tplot+'.tplot' 
     GET_DATA, 'data', data = saved_data
     data = saved_data.data
  ENDIF ELSE BEGIN
     input_data = READ_DAILY_CSV_rbsp(jd_s, ndays, data_path,ts,te, avoid_compression_time = avoid_compression_time , avoid_2019 = avoid_2019)
     data = input_data.data

     data = calculate_daily_data(data)

     IF KEYWORD_SET(store_tplot) THEN BEGIN
        STORE_DATA, 'data', data = {data:data}
        SPAWN, 'mkdir -p '+ FILE_DIRNAME(fln_saved_tplot)
        TPLOT_SAVE, 'data', filename = fln_saved_tplot
        header = TAG_NAMES(data)
        WRITE_CSV, fln_saved_tplot+'.csv', data, HEADER = header
     ENDIF  
  ENDELSE

  RETURN, data
END
