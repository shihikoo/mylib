PRO calculate_velocity_from_energy, energy, sp, vel 
;-- Constants --
  Avogadro_constant = 6.02214086e23 ; moe-1
  electron_charge = 1.60217662e-19  ;coulombs
; Ion mass in amu
  CASE sp OF                      
     0: BEGIN                                                  
        ion_mass = 1.0              
        sp_str = 'h'                  
     END                                      
     1: BEGIN                                     
        ion_mass = 4.002602/2. 
        sp_str = 'he1'                     
     END
     2: BEGIN                                                          
        ion_mass = 4.002602                  
        sp_str = 'he2'                                
     END
     3: BEGIN                                            
        ion_mass = 15.89                                                 
        sp_str = 'o'                  
     END                                  
     4: BEGIN                                                              
        ion_mass = 1./1837.                                   
        sp_str = 'e'                                                       
     END                                                                        
  ENDCASE    

  vel = sqrt(2.*energy*electron_charge/(ion_mass/Avogadro_constant/1e3))/1e3 ;4.577e7*sqrt(data_energy/1e3/AMU) 

;  (vel * 1.e3)^2/2./electron_charge*((ion_mass/Avogadro_constant/1e3) = energy

END
