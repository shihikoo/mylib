;-------------------------------------------------------------------------------
; Purpose: change_tplot_name
;
; Inputs:
;       old_name : old tplot name
;       new_name   : new tplot name
;
; Output: Depends on Keywords settings. The possible output includes
; data, tplot file and plots. There will also be two .log files
;
; Written by Jing Liao  03/10/2023
;-------------------------------------------------------------------------------
PRO change_tplot_name, old_name, new_name
  get_data,old_name, data=data,dlim=dlim,lim=lim
  store_data,new_name, data=data,dlim=dlim,lim=lim
  store_data,delete=old_name
END 
