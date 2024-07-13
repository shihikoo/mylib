pro calculate_para_velocity, vel_name, mag_name, para_name, perp_name
    get_data, vel_name, data = data, dlim = dlim, lim = lim
    
    time = data.x
    vx = data.y[*,0]
    vy = data.y[*,1]
    vz = data.y[*,2]

    b = r_data(mag_name, /Y)
    bx = b[*,0]
    by = b[*,1] 
    bz = b[*,2] 
    
    const = (vx*bx + vy*by + vz*bz)/abs(bx^2+by^2+bz^2)

    vpara_x = const * Bx
    vpara_y = const * By 
    vpara_z = const * bz
    vpara = sqrt(vpara_x^2 +vpara_y^2 +vpara_z^2  )

    store_data, para_name, data = {x:time, y:vpara}, dlim=dlim, lim=lim

    vperp_x = vx - vpara_x
    vperp_y = vy - vpara_y 
    vperp_z = vz - vpara_z
    vperp = sqrt(vpara_x^2 +vpara_y^2 +vpara_z^2  )

    store_data, perp_name, data = {x:time, y:vperp}, dlim=dlim, lim=lim

end