function convert_matrix_to_structure, matrix, header, data_appendto = data_appendto
  nterm1 = (size(matrix))[2]
  nterm2 = n_elements(header)

  if nterm1 ne nterm2 then begin
     print, "matrix and header do not have  the same column dimention"
     stop
  endif else nterm = nterm1

  n_time = (size(matrix))[1]
  
  for iterm = 0, nterm-1 do begin
     if ~keyword_set(data_appendto) then data_appendto = CREATE_STRUCT(header[iterm], matrix[*,iterm]) else data_appendto = CREATE_STRUCT(data_appendto, header[iterm], matrix[*,iterm])
  endfor 
  return, data_appendto
end 
