;--------------------------------
; find local maximum in 1D array
;---------------------------------

function local_max, datay,   minima = minima ;keyword for minimum /minima
                                ;initialize list
  max_points = list()
  
  data_y = datay
                                ;check for keyword, flip the sign of the y values
  if keyword_set(minima) then data_y = -datay

                                ;iterate through elements
  for i=1, n_elements(data_y)-2 do begin
                                ;previous point less than i-th point and next point less than i-th point
     if ( (data_y[i-1] lt data_y[i]) AND (data_y[i] gt data_y[i+1])) then max_points.add, i
  endfor
  
                                ;return an array of the indices where the extrema occur
  return, max_points.toarray()
end
