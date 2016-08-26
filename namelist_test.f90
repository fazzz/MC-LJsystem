program Namelist_test
  use initialize
  use generate_candidate
  use pbs
  use read_inputs
  use write_outputs
  use energy
!  use neibhoringlist
  use mtmod

  implicit none

  integer i,j,k

  integer Num, maxsteps, outputinterval, listinterval, seed
  ! data Num,maxsteps,outputinterval,listinterval,seed/108,10000,10,10,4358/
  ! Num: The number of atoms in the system.
  ! maxsteps: The number of steps for MC calculatin.
  ! outputinterbal: The frequency of steps to save the coordinate in the output file.
  ! listinterval: The frequenct of steps to re-calculate the neibhoring list.
  ! seed: The seed for random number generators.
  double precision T, epsilon, sigma, L_box_size, L_cut_off, deltaX, deltaS
  ! data T,epsilon,sigma,L_box_size,L_cut_off,deltaX,deltaS/328.0e0,119.8e0,3.822e0,30.0e0,10.0e0,0.1e0,1.0e0/
  ! T: The reduced temperature of the system.
  ! epsilon: The force constant of the Lenord-Jones potential.
  ! sigma: The collision radius of the atom in the system.
  ! L_box_size: The length of the box.
  ! L_cut_off: The length of the cutt off.
  ! deltaX: The maximum moving length of the each step.
  ! deltaS: The maximum difference of neibhoring list.

  integer :: ON=1
  integer :: OFF=0

  double precision, allocatable,dimension(:,:) :: r, r_candidate
  double precision e_total, e_total_candidate, r_d, u, beta

  integer M
  integer, allocatable :: P_list, L_list

  double precision acceptance_ratio
  data acceptance_ratio/0.0e0/

  integer initializeMODE
  integer time_start, time_finish, time

  character coordinate_filename*50
  character outputfilename_coordinate*50
  character outputfilename_energy*50

  character(10) :: argv, programname
  integer :: argc, iargc

!  initializeMODE=ON

  NAMELIST /mccontrols/ Num, maxsteps, outputinterval, seed, T, epsilon, sigma, L_box_size, L_cut_off, deltaX
  open(17,file='parameters_mc')                                                                                      
  read(17,mccontrols)                                                                                            
  close(17)                                                                                                          

  argc=iargc()
  call getarg(0,programname)
  
  if (argc < 2) then
!     call usage(programname)
  else if ( argc == 2 ) then
     call getarg(1,outputfilename_coordinate)
     call getarg(2,outputfilename_energy)
  else if ( argc == 3 ) then
     call getarg(1,coordinate_filename)
     call getarg(2,outputfilename_coordinate)
     call getarg(3,outputfilename_energy)
!     initializeMODE = OFF
!  else
!     call usage(programname)
  end if
  
  print *,Num, maxsteps, outputinterval, seed, T, epsilon, sigma, L_box_size, L_cut_off, deltaX

end program Namelist_test
