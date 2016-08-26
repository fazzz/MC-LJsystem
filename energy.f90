! This module calculate Leneord-Jones potential
! with periodic bounary conditions.
module energy
  implicit none

! double precision function len(r_i, r_j)
! double precision function energy_LJ(r_ij, sigma, epsilon)
! double precisoon function total_energy_LJ(r, N, L_box_size, epsilon, sigma, cut_off)

contains
  double precision function len(r_i, r_j)
    implicit none
    double precision,dimension(3),intent(in) :: r_i, r_j
    
    integer i

    len = 0.0d0
    do i=1,3,1
       len = len +  (r_i(i)-r_j(i))*(r_i(i)-r_j(i))
    end do
    
    len = sqrt(len)
  end function len

  double precision function energy_LJ(r_ij, sigma, epsilon)
    implicit none                                       
    double precision,intent(in) :: r_ij, sigma, epsilon 
                                                        
    double precision r_ij_sigma,r_ij_sigma_12,r_ij_sigma_6
                                                          
    r_ij_sigma = r_ij/sigma                               
    r_ij_sigma_12 = r_ij_sigma**(-12.0)                   
    r_ij_sigma_6  = r_ij_sigma**(-6.0)                    
                                                          
    energy_LJ = epsilon*(r_ij_sigma_12 - 2.0d0*r_ij_sigma_6) 
  end function energy_LJ
                                                          
  double precision function total_energy_LJ(r, N, L_box_size, epsilon, sigma, L_cut_off) 
    implicit none

    integer , intent(in) :: N
    double precision, allocatable,dimension(:,:), intent(in) :: r
    double precision, intent(in) :: L_box_size, sigma, epsilon, L_cut_off

    integer i, j, v_x, v_y, v_z
    double precision e_ij
    double precision r_ij, r_ij_mi, r_j_image(3)

    total_energy_LJ = 0.0d0
    do i=1,N,1
       do j=i+1,N,1

          r_ij_mi=L_cut_off
          do v_x=-1,1,1    
             do v_y=-1,1,1 
                do v_z=-1,1,1
                   r_j_image(1) = r(j,1) + L_box_size*v_x
                   r_j_image(2) = r(j,2) + L_box_size*v_y
                   r_j_image(3) = r(j,3) + L_box_size*v_z
                   r_ij = len(r(i,:),r(j,:))             
                  if ( r_ij < r_ij_mi ) then
                     r_ij_mi = r_ij         
                  end if
               end do                       
            end do                          
         end do
                                                                                         
         if ( r_ij_mi < L_cut_off ) then
            e_ij = energy_LJ(r_ij_mi, sigma, epsilon)
            e_ij = energy_LJ(r_ij, sigma, epsilon)     
            total_energy_LJ = total_energy_LJ + e_ij   
         end if
      end do
   end do
  end function total_energy_LJ
end module energy

