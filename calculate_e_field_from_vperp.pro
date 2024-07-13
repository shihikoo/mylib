PRO calculate_e_field_from_vperp, vperp_name, bt_name, Et_name

get_data, vperp_name, data=data
vperp = data.y

get_data, bt_name, data = data
bt = data.y

Et = (vperp * bt) * 1e3 * 1e-9 * 1e3  ;mV/m  E=-VXB
store_data, Et_name, data = {x:data.x, y:Et}

;; if ~KEYWORD_SET(use_proton) then begin
;;     E_t=(V_perp_tail * B) * 1e3 * 1e-9 * 1e3  ;mV/m  E=-VXB
;;     EXB_t=E_t*B/B^2 * 1e-3*1e9*1e-3 ;km/s V=EXB
;; endif else begin
;;     Ex = -(proton_Vy * Bz - Proton_Vz * By) * 1e3 * 1e-9 * 1e3 ;mV/m  E=VXB
;;     Ey = -(proton_Vz * Bx - Proton_Vx * Bz) * 1e3 * 1e-9 * 1e3
;;     Ez = -(proton_Vx * By - Proton_Vy * Bx) * 1e3 * 1e-9 * 1e3 

;;     E = [[Ex],[Ey],[Ez]]
;;     e_t=sqrt(ex^2+ey^2+ez^2)

;;     EXB_x= -(Ey * Bz - Ez * By)/b^2*(1e3*1e9*1e-3*1e-3) ;km/s drift velocity EXB
;;     EXB_y= -(Ez * Bx - Ex * Bz)/b^2*(1e3*1e9*1e-3*1e-3)
;;     EXB_z= -(Ex * By - Ey * Bx)/b^2*(1e3*1e9*1e-3*1e-3)

;;     EXB=[[EXB_X],[EXB_Y],[EXB_Z]]
;;     exb=sqrt(exb_x^2+exb_y^2+exb_z^2)
;; endelse 

END
