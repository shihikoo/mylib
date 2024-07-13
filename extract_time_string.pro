FUNCTION extract_time_string, ts
  time_s = STRMID(ts, 11, 2) + STRMID(ts, 14, 2) + STRMID(ts, 17, 2)    
  RETURN, time_s
END
